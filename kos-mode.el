;;; kos-mode.el --- Summary
;; Copyright (C) 2015  Fabian Kantereit
;; Author: Fabian Kantereit <fkantere@E6530-FKANTERE.INFORM-AC>
;; Keywords:
;;; Commentary:
;;
;;
;;; Code:

;; (require 'regexp-opt)
;;;;;;;;;;;;;;;
;; Mode hook ;;
;;;;;;;;;;;;;;;
(defvar kos-mode-hook nil)

;;;;;;;;;;;;;;
;; Mode map ;;
;;;;;;;;;;;;;;
(defvar kos-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for kOS major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ks\\'" . kos-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Language Syntax ;;
;;;;;;;;;;;;;;;;;;;;;

(defconst kos-font-lock-keywords-1
  (list
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined Keywords ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; SHIP, STAGE an TARGET
   '("\\<\\(\\(SHIP\\|STAGE\\|TARGET\\|TIME\\|ENCOUNTER\\|CONFIG\\):\\(\\w*:?\\)*\\)\\>" . font-lock-keyword-face)
   ;; ALT and ETA
   '("\\<\\(ALT:\\(APOAPSIS\\|PERIAPSIS\\|RADAR\\)\\|ETA:\\(APOAPSIS\\|PERIAPSIS\\|TRANSITION\\)\\)\\>" . font-lock-keyword-face)
   ;; GENERAL
   `(,(regexp-opt '("UP" "PROGRAGE" "RETROGRADE" "FACING" "MAXTHRUST" "VELOCITY" "GEOPOSITION" "THROTTLE" "STEERING" "STAGE" "SHIP" "HEADING" "LATITUDE" "LONGITUDE" "NORTH" "BODY" "ANGULARMOMENTUM" "ANGULARVEL" "ANGULARVELOCITY" "COMMRANGE" "MASS" "VERTICALSPEED" "SURFACESPEED" "AIRSPEED" "VESSELNAME" "ALTITUDE" "APOAPSIS" "PERIAPSIS" "SENSORS" "SRFPROGRADE" "SRFREROGRADE" "OBT" "STATUS" "WHEELTHROTTLE" "WHEELSTEERING" "SAS" "RCS" "GEAR" "LEGS" "CHUTES" "LIGHTS" "PANELS" "BRAKES" "ABORT" "VERSION" "VERSION:MAJOR" "VERSION:MINOR" "SESSIONTIME" "TIME" "MAPVIEW" "WARP" "WARPMODE") 'words) . font-lock-keyword-face)
   ;; Action Groups
   '("\\<AG[0-9]+\\>" . font-lock-keyword-face)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtin Language Features ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   `(,(regexp-opt '("SET" "TO" "IF" "ELSE" "UNTIL" "LOCK" "UNLOCK" "PRINT" "AT" "TOGGLE"
                         "WAIT" "WHEN" "THEN"  "OFF"  "CLEARSCREEN"  "ADD" "REMOVE"  "LOG"
                         "BREAK" "PRESERVE" "DECLARE" "PARAMETER" "SWITCH"  "COPY"  "FROM" "RENAME"
                         "VOLUME"  "FILE"  "DELETE"  "EDIT"  "RUN" "COMPILE" "LIST" "REBOOT" "SHUTDOWN"
                         "FOR" "UNSET" "BATCH" "DEPLOY" "IN" "ALL") 'words) . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for kOS mode")

(defconst kos-font-lock-keywords-2
  (append kos-font-lock-keywords-1
          (list
           '("\\<\\(ON\\|OFF\\|TRUE\\|FALSE\\)\\>" . font-lock-constant-face) ;; ON / OFF keywords
           '("\"[^\"]+\"" . font-lock-string-face) ;; string literals
           '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face) ;; integer literals
           '("\\b[-+]?[0-9]+\.[0-9]*\([eE][-+]?[0-9]+\)?\\b" . font-lock-preprocessor-face) ;; floating point literals
           )
          )
  "Additional Keywords to highlight in kOS (KerbalScript) mode")

(defvar kos-font-lock-keywords kos-font-lock-keywords-2
  "Default highlighting expressions for kOS (KerbalScript) mode")

(defvar kos-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for kos-mode")

(define-derived-mode kos-mode prog-mode "KerboScript"
  "Major mode for editing KerboScript Files"
  :syntax-table kos-mode-syntax-table
  (use-local-map kos-mode-map)
  ;; set syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '(kos-font-lock-keywords nil t))
  ;; this should be case insensitive
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  (setq comment-start "// ")
  (setq comment-end "")
  (run-hooks 'kos-mode-hook)
  )

(provide 'kos-mode)

;;; kos-mode.el ends here
