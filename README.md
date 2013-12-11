wooky.el
========

Eval-defun for JavaScript in Chrome

![Imgur](http://i.imgur.com/uMJNFjE.png)

## Usage

Open your Chrome with remote debugging enabled:

    open -a /Applications/Google\ Chrome.app \
    --args "--remote-debugging-port=9222"

Require the package:

    (require 'wooky)

In your buffer of choice run the command:

    <M-x> wooky-mode
