;;; wooky.el --- a minor mode to connect to Chrome DevTools -*- lexical-binding: t -*-

;; Copyright (C) 2013 katspaugh

;; Author: katspaugh
;; Keywords: tools
;; URL: https://github.com/katspaugh/wooky.el
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3") (json "1.4") (websocket "1.1") (js2-mode "20150524"))

;;; Usage:

;; Open your Chrome with remote debugging enabled:
;;
;;     open -a "Google Chrome" --args "--remote-debugging-port=9222"

;; Require the package:
;;
;;     (require 'wooky)

;; In your buffer of choice run the command:
;;
;;     <M-x> wooky-mode

;; <C-x w e> to evaluate region or expression at point
;; <C-x w u> to live-update the script

;;; Code:

(require 'url)
(require 'json)
(require 'websocket)
(require 'js2-mode)

;;; Variables

(defcustom wooky--default-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'wooky-mode)

(defcustom wooky--default-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'wooky-mode)

(defvar wooky--rpc-id 0
  "Id for DevTools calls.")

(defvar-local wooky--socket nil
  "Connected socket.")

(defvar-local wooky--scripts nil
  "List of JavaScript files available for live editing.")

;;; Hooks

(let ((obarray '()))
  (defun wooky--add-hook (id func)
    (let ((hook (intern (number-to-string id) obarray)))
      (add-hook hook func t)))

  (defun wooky--run-hook (id data)
    (let ((hook (intern-soft (number-to-string id) obarray)))
      (when hook
        (run-hook-with-args hook data)
        (unintern hook obarray)))))

;;; Connection

(defun wooky--encode (packet)
  "Encodes PACKET into JSON."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode packet)))

(defun wooky--decode (packet)
  "Decodes PACKET from JSON."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string packet)))

(defun wooky--get-rpc-id ()
  "Get unique id."
  (setq wooky--rpc-id (1+ wooky--rpc-id)))

(defun wooky--call-rpc (method &optional params callback)
  "Call a DevTools METHOD with optional PARAMS and CALLBACK."
  (let ((id (wooky--get-rpc-id)))
    (when callback
      (wooky--add-hook id callback))
    (websocket-send-text
     wooky--socket
     (wooky--encode (list :id id
                          :method method
                          :params params)))))

(defun wooky--on-open (buf sock)
  "Run when buffer BUF opens a socket SOCK."
  (message "Wooky: socket opened")
  (with-current-buffer buf
    (setq wooky--socket sock)
    (wooky--call-rpc "Debugger.enable")))

(defun wooky--on-eval (data)
  "Display results DATA of evaluated expression."
  (let* ((res (plist-get data :result))
         (was-thrown (plist-get res :wasThrown))
         (result (plist-get res :result))
         (type (plist-get result :type))
         (value (plist-get result :value))
         (descr (plist-get result :description)))
    (let ((msg (if (eq json-false was-thrown)
                   (if (string-equal "object" type)
                       (wooky--encode value) (or value descr type))
                 descr)))
      (when msg (message "%s" msg)))))

(defun wooky--on-script-parsed (data)
  "Run when a parsed script DATA comes."
  (let* ((params (plist-get data :params))
         (extensionp (plist-get params :isContentScript))
         (url (plist-get params :url))
         (id (plist-get params :scriptId)))
    (when (and url (not extensionp))
      (add-to-list 'wooky--scripts (list :id id :url url)))))

(defun wooky--on-message (buf sock frame)
  "Receive a message in buffer BUF from socket SOCK in FRAME."
  (with-current-buffer buf
    (let* ((data (wooky--decode (websocket-frame-payload frame)))
           (method (plist-get data :method))
           (id (plist-get data :id)))
      (cond
       ((string-equal method "Debugger.scriptParsed")
        (wooky--on-script-parsed data))
       ((numberp id)
        (wooky--run-hook id data))))))

(defun wooky--on-close (buf sock)
  "Run when in buffer BUF a socket SOCK is closed."
  (message "Wooky: socket closed")
  (with-current-buffer buf
    (setq wooky--socket nil)
    (wooky-mode -1)))

(defun wooky--websocket-open (url id)
  "Open the Web Socket URL with ID."
  (let ((buf (current-buffer)))
    (websocket-open
     url
     :on-open (apply-partially #'wooky--on-open buf)
     :on-message (apply-partially #'wooky--on-message buf)
     :on-close (apply-partially #'wooky--on-close buf))))

(defun wooky--get-tabs (host port)
  "Get opened Chrome tabs from HOST on PORT."
  (let* ((url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (url (url-parse-make-urlobj
               "http" nil nil host port "/json")))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (eq 200 (url-http-parse-response))
          (progn
            (goto-char (+ 1 url-http-end-of-headers))
            (let ((json-array-type 'list)
                  (json-object-type 'plist))
              (json-read)))
        (message "Wooky: cannot connect to %s:%d" host port)
        (wooky-mode -1)))))

(defun wooky--get-completions (tabs)
  "Display available Chrome TABS."
  (let ((candidates))
    (mapc
     (lambda (tab)
       (let ((url (plist-get tab :url)))
         (when (string-match "^http" url)
           (push (cons url tab)
                 candidates))))
     tabs) candidates))

(defun wooky--connect (host port)
  "Connect to HOST on PORT."
  (let ((tabs (wooky--get-tabs host port)))
    (when tabs
      (let ((completions (wooky--get-completions tabs)))
        (let ((choice (completing-read
                       "Choose a tab: "
                       completions nil t "" nil (caar completions))))
          (let ((socket-info (cdr (assoc choice completions))))
            (wooky--websocket-open
             (plist-get socket-info :webSocketDebuggerUrl)
             (plist-get socket-info :id))))))))

(defun wooky--script-id (file)
  "Get id of the script corresponding to FILE."
  (let ((name (file-name-base file))
        id)
    (mapc
     (lambda (script)
       (when (string= name (file-name-base (plist-get script :url)))
         (setq id (plist-get script :id))))
     wooky--scripts)
    id))

;;; Flash

(defun wooky--flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))


;;; Find function bounds at point

;; See https://github.com/swank-js/swank-js

(defun wooky--start-of-toplevel-form ()
  "Beginning of top-level form."
  (when js2-mode-buffer-dirty-p
    (js2-mode-wait-for-parse #'wooky--start-of-toplevel-form))
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

(defun wooky--end-of-toplevel-form ()
  "End of top-level form."
  (js2-forward-sws)
  (let ((node (js2-node-at-point)))
    (unless (or (null node) (js2-ast-root-p node))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-end node)))
    (point)))


;;; Minor mode

(defun wooky-update ()
  "Send the script in the updated current buffer to DevTools."
  (interactive)
  (let ((id (wooky--script-id (buffer-file-name))))
    (if id
        (let ((source (buffer-substring-no-properties
                       (point-min) (point-max))))
          (wooky--call-rpc
           "Debugger.setScriptSource"
           (list :scriptId id :scriptSource source))
          (message "Wooky: updating script source..."))
      (message "Wooky: no matching script for current buffer"))))

(defun wooky-eval ()
  "Evaluates region or function at point."
  (interactive)
  (let (start end)
    (if (region-active-p)
        (setq start (region-beginning)
              end (region-end))
      (setq start (wooky--start-of-toplevel-form)
            end (wooky--end-of-toplevel-form)))
    (wooky--flash-region start end)
    (wooky--call-rpc
     "Runtime.evaluate"
     (list
      :expression
      (buffer-substring-no-properties start end)
      :returnByValue t)
     #'wooky--on-eval)))

(defvar wooky-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x w u") 'wooky-update)
    (define-key map (kbd "C-x w e") 'wooky-eval)
    map)
  "Key-map used when `wooky-mode' is active.")

(defun wooky-mode-enter ()
  "Enter wooky-mode."
  (add-hook 'kill-buffer-hook 'wooky-mode-exit nil t)
  (wooky--connect wooky--default-remote-host
                  wooky--default-remote-port))

(defun wooky-mode-exit ()
  "Exit wooky-mode."
  (remove-hook 'kill-buffer-hook 'wooky-mode-exit t)
  (when wooky--socket
    (websocket-close wooky--socket)
    (setq wooky--socket nil))
  (setq wooky--scripts nil))

;;;###autoload
(define-minor-mode wooky-mode
  "Minor mode to evaluate JavaScript in a remote WebKit debugger."
  :global nil :group 'wooky :init-value nil :lighter " Wooky"
  (if wooky-mode
      (wooky-mode-enter)
    (wooky-mode-exit)))

(provide 'wooky)

;;; wooky.el ends here
