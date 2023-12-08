;;; -*- lexical-binding: t; -*-

(setq
 erc-log-channels-directory "~/.erc/logs/"
 erc-save-buffer-on-part nil
 erc-save-queries-on-quit nil
 erc-log-write-after-send t
 erc-log-write-after-insert t
 erc-rename-buffers t
 erc-nick "mehak"
 erc-server "irc.libera.chat"
 erc-port 6697
 erc-fill-column 140
 erc-join-buffer 'bury
 erc-timestamp-only-if-changed-flag nil
 erc-timestamp-format "[%Y-%m-%d %H:%M]"
 erc-insert-timestamp-function 'erc-insert-timestamp-left)
