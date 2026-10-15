(uiop:define-package #:slt
  (:use #:cl)
  (:export
   ;; Hooks and configuration
   #:define-hook
   #:add-hook
   #:remove-hook
   #:run-hook
   #:*after-init-hook*
   #:*title-change-hook*
   #:*bell-hook*
   #:*key-hook*
   #:*exit-hook*
   #:*load-user-config*
   #:user-config-pathname
   #:load-user-config
   ;; Grid
   #:grid
   #:make-grid
   #:grid-rows
   #:grid-columns
   #:grid-codes
   #:grid-fg
   #:grid-bg
   #:grid-attrs
   #:grid-dirty
   #:grid-index
   #:grid-cursor-row
   #:grid-cursor-column
   #:grid-cursor-visible-p
   #:grid-cursor-shape
   #:grid-cell-string
   #:grid-row-text
   #:grid-text
   #:grid-dirty-rows
   #:grid-clear-dirty
   #:grid-mark-all-dirty
   #:attributes->list
   #:list->attributes
   #:palette-color
   #:rgb-color
   #:+color-default+
   ;; Palette
   #:palette
   #:make-palette
   #:*palette*
   #:palette-foreground
   #:palette-background
   #:palette-cursor
   #:palette-colors
   #:cell-rgb
   #:rgb->hex
   #:hex->rgb
   ;; Engines
   #:engine
   #:*default-engine*
   #:register-engine
   #:available-engines
   #:make-engine
   #:engine-rows
   #:engine-columns
   #:engine-feed
   #:engine-feed-string
   #:engine-resize
   #:engine-update-grid
   #:engine-mode
   #:engine-encode-key
   #:engine-close
   #:engine-emit-output
   #:engine-emit-title
   #:engine-emit-bell
   #:engine-output-function
   #:engine-title-function
   #:engine-bell-function
   #:xterm-encode-key
   ;; Backends
   #:terminal-backend
   #:register-backend
   #:available-backends
   #:make-backend
   #:backend-render-grid
   ;; Frontend
   #:launch-terminal
   #:terminal-emulator
   #:write-input
   #:render-emulator
   #:resize-emulator
   #:fit-grid-to-size
   #:translate-key-event
   #:keysym->terminal-key
   #:decode-modifier-state
   ;; Transport
   #:launch-shell-process
   #:process-alive-p
   #:terminate-process
   #:resize-process-pty
   #:read-available-output
   #:read-available-octets
   #:write-octets
   #:native-pty-supported-p
   ;; Fonts
   #:default-font-family
   #:pick-font-family
   #:resolve-font-family
   #:resolve-cell-dimensions
   #:terminal-font-size
   ;; CLI and images
   #:main
   #:make-cli-command
   #:parse-cli-arguments
   #:command-launch-arguments
   #:dump-executable
   #:default-executable-name
   #:default-executable-output
   #:normalize-executable-output))
