;;; -*- lexical-binding: t -*-
;;; wooky.el --- a minor mode to evaluate JavaScript in WebKit

;; Copyright (C) 2013 katspaugh

;; Author: katspaugh
;; Keywords: tools
;; URL: https://github.com/katspaugh/.emacs.d
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (js2-mode "20130619") (websocket "1.1"))

;;; Commentary:

;; Being minimalistic, Wooky is focused only on evaling
;; separate expressions/regions and displaying the result
;; in the message area.

;; Wooky relies on js2-mode to get expression under the cursor.

;;; Credits:

;; Most of the code is a shameless rip off the amazing Kite.
;; Wooky can do only a tiny bit of what Kite can.
;; See http://github.com/jscheid/kite for more.

;;; Usage:

;; Open your Chrome with remote debugging enabled:
;;
;;     open -a /Applications/Google\ Chrome.app \
;;     --args "--remote-debugging-port=9222"

;; Require the package:
;;
;;     (require 'wooky)

;; In your buffer of choice run the command:
;;
;;     <M-x> wooky-mode

;;; Code:

(require 'url)
(require 'json)
(require 'websocket)

(defcustom wooky-default-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'wooky-mode)

(defcustom wooky-default-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'wooky-mode)

(defvar wooky-tab-history nil
  "Keeps debugger tabs history.")

(defvar wooky-expressions-history nil
  "Keeps evaluated expressions history.")

(defvar wooky-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'wooky-eval-defun)
    (define-key map (kbd "C-x r") 'wooky-reload-page)
    map)
  "Keymap used when `wooky-mode' is active.")

;; Sockets
(defvar wooky-sockets-alist nil "Inter-buffer list of websockets with IDs.")
(defvar wooky-socket nil "Buffer-local socket reference.")
(defvar wooky-buffers nil "List of connected buffers.")

(defun wooky-socket-send (packet)
  (websocket-send-text wooky-socket (wooky-encode packet)))

(let ((id 0))
  (defun wooky-next-rpc-id ()
    (incf id)))

(let ((obarray '()))
  (defun wooky-add-hook (id func)
    (let ((hook (intern (number-to-string id) obarray)))
      (add-hook hook func t)))

  (defun wooky-run-hook (id data)
    (let ((hook (intern-soft (number-to-string id) obarray)))
      (when hook
        (run-hook-with-args hook data)
        (unintern hook obarray)))))

(defun wooky-encode (packet)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode packet)))

(defun wooky-decode (json-str)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string json-str)))

(defun wooky-send-evaluate (expression &optional callback)
  (let ((id (wooky-next-rpc-id)))
    (when callback (wooky-add-hook id callback))
    (wooky-socket-send
     (list :id id
           :method "Runtime.evaluate"
           :params (list :expression expression
                         :returnByValue t)))))

(defun wooky-get-tabs (&optional host port)
  (let* ((url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (url (url-parse-make-urlobj
               "http" nil nil host port "/json")))
    (with-current-buffer (url-retrieve-synchronously url)
      (when (eq 200 (url-http-parse-response))
        (goto-char (+ 1 url-http-end-of-headers))
        (let ((json-array-type 'list)
              (json-object-type 'plist))
          (json-read))))))

(defun wooky-get-completions (tabs)
  (let ((index 0) candidates)
    (mapc
     (lambda (tab)
       (let ((url (plist-get tab :url)))
         (when (string-match "^http" url)
           (push (cons (format "%s (%s)" url (incf index)) tab)
                 candidates))))
     tabs) candidates))

(defun wooky-connect (host port)
  (let ((tabs (wooky-get-tabs host port)))
    (if tabs
        (let* ((completions (wooky-get-completions tabs))
               (choice (completing-read
                        "Choose a tab: "
                        completions nil t ""
                        'wooky-tab-history
                        (caar completions)))
               (socket-info (cdr (assoc choice completions)))
               (socket-id (plist-get socket-info :id))
               (socket-url (plist-get socket-info :webSocketDebuggerUrl)))
          (cond (socket-url
                 (wooky-websocket-open socket-url socket-id))
                (socket-id
                 (let ((sock (cdr (assoc socket-id wooky-sockets-alist))))
                   (message "Wooky: connected to previously opened socket")
                   (wooky-set-socket sock)))))
      (progn
        (wooky-mode -1)
        (message "Wooky: cannot connect to %s:%d" host port)))))

(defun wooky-websocket-open (url id)
  (let ((buf (current-buffer)))
    (websocket-open
     url
     :on-open
     (lambda (sock)
       (message "Wooky: socket opened")
       (add-to-list 'wooky-sockets-alist (cons id sock))
       (with-current-buffer buf
         (wooky-set-socket sock)))
     :on-message
     (lambda (sock frame)
       (let ((data (wooky-decode (websocket-frame-payload frame))))
         (when (plist-get data :result)
           (wooky-run-hook (plist-get data :id) data))))
     :on-close
     (lambda (sock)
       (message "Wooky: socket closed")
       (wooky-release-buffers sock)))))

;; From swank-js, originally called slime-js-start-of-toplevel-form.
;; https://github.com/swank-js/swank-js/slime-js.el
(defun wooky-start-of-toplevel-form ()
  (when js2-mode-buffer-dirty-p
    (js2-mode-wait-for-parse #'wooky-start-of-toplevel-form))
  (js2-forward-sws)
  (if (= (point) (point-max))
      (js2-mode-forward-sexp -1)
    (let ((node (js2-node-at-point)))
      (when (or (null node)
                (js2-ast-root-p node))
        (error "Wooky: cannot locate any toplevel form"))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-pos node))
      (js2-forward-sws)))
  (point))

;; From swank-js, originally called slime-js-start-of-toplevel-form.
;; https://github.com/swank-js/swank-js/slime-js.el
(defun wooky-end-of-toplevel-form ()
  (js2-forward-sws)
  (let ((node (js2-node-at-point)))
    (unless (or (null node) (js2-ast-root-p node))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-end node)))
    (point)))

;; From SLIME, originally called slime-flash-region.
;; http://common-lisp.net/viewvc/slime/slime/slime.el?view=markup
(defun wooky-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun wooky-eval-expression ()
  (interactive)
  (wooky-send-evaluate
   (read-from-minibuffer
    "Expression: " (first wooky-expressions-history))
   #'wooky-message-result))

(defun wooky-reload-page ()
  (interactive)
  (message "Wooky: reloaded the connected tab")
  (wooky-socket-send
   (list
    :id (wooky-next-rpc-id)
    :method "Page.reload"
    :params (list :ignoreCache t))))

(defun wooky-eval (start end)
  (wooky-flash-region start end)
  (wooky-send-evaluate
   (buffer-substring-no-properties start end)
   #'wooky-message-result))

(defun wooky-eval-region ()
  (interactive)
  (wooky-eval (region-beginning) (region-end)))

(defun wooky-eval-defun ()
  (interactive)
  (wooky-eval (wooky-start-of-toplevel-form) (wooky-end-of-toplevel-form)))

(defun wooky-message-result (data)
  (let* ((res (plist-get data :result))
         (was-thrown (plist-get res :wasThrown))
         (result (plist-get res :result))
         (type (plist-get result :type))
         (value (plist-get result :value))
         (descr (plist-get result :description)))
    (let ((msg (if (eq json-false was-thrown)
                   (if (string-equal "object" type)
                       (wooky-encode value) (or value descr type))
                 descr)))
      (when msg (message "%s" msg)))))

(defun wooky-release-socket ()
  (setq wooky-buffers (remove (current-buffer) wooky-buffers))
  (unless wooky-buffers
    (let ((sock wooky-socket))
      (setq wooky-socket nil)
      (websocket-close sock))))

(defun wooky-release-buffers (sock)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq sock wooky-socket)
              (setq wooky-buffers (remove buf wooky-buffers))
              (setq wooky-socket nil)
              (wooky-mode -1))))
        wooky-buffers))

(defun wooky-set-socket (sock)
  (setq wooky-socket sock)
  (add-to-list 'wooky-buffers (current-buffer)))

(defun wooky-mode-enter (&optional custom-host-port)
  (make-local-variable 'wooky-socket)
  ;; When the buffer is killed, exit the mode
  (add-hook 'kill-buffer-hook 'wooky-mode-exit nil t)
  ;; Connect to websocket
  (let ((host wooky-default-remote-host)
        (port wooky-default-remote-port))
    (when custom-host-port
      (setq host (completing-read "Debugger host: " (list host)
                                  nil nil nil nil host))
      (setq port (string-to-number
                  (completing-read "Debugger port: "
                                   (list (number-to-string port))
                                   nil nil nil nil port))))
    (wooky-connect host port)))

(defun wooky-mode-exit ()
  (remove-hook 'kill-buffer-hook 'wooky-mode-exit t)
  (when wooky-socket
    (wooky-release-socket)))

(define-minor-mode wooky-mode
  "Minor mode to evaluate JavaScript in a remote WebKit debugger."
  :global nil :group 'wooky :init-value nil :lighter " Wooky"
  (if wooky-mode
      (wooky-mode-enter current-prefix-arg)
    (wooky-mode-exit)))

(provide 'wooky)

;;; wooky.el ends here
