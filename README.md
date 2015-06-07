wooky.el
========

Eval JavaScript expressions and live-reload scriprs in Chrome from
Emacs buffers.

![Imgur](http://i.imgur.com/uMJNFjE.png)

## Usage

Open your Chrome with remote debugging enabled:

     open -a "Google Chrome" --args "--remote-debugging-port=9222"

Require the package:

    (require 'wooky)

In your buffer of choice run the command:

    <M-x> wooky-mode

 * <kbd>C-x w e</kbd> to evaluate region or expression at point
 * <kbd>C-x w u</kbd> to live-update the script
