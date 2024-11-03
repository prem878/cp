(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(ido-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages '(evil auto-complete flycheck gruber-darker-theme smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(set-face-attribute 'default nil :height 170)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq-default tab-width 4)
(electric-pair-mode 1)
;; Intellisense syntax checking
;; http://www.flycheck.org/en/latest/
;; Set up C/C++ indentation to 4 spaces
(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4)    ; Set indentation to 4 spaces
  (setq indent-tabs-mode nil) ; Use spaces instead of tabs
  (setq tab-width 4))         ; Set tab width to 4 spaces

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(setq compile-command "cd build && cmake .. && make")
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)
;; cursor don't fuck up
;; Set cursor type for both normal and insert modes
(setq evil-normal-state-cursor '(box "yellow"))  ; Change "yellow" to your preferred color
(setq evil-insert-state-cursor '(box "yellow"))  ; Change "yellow" to your preferred color
(defvar my-evil-mode-indicator "Normal"
  "Indicator for the current Evil mode.")

(defun my-update-evil-mode-indicator ()
  (setq my-evil-mode-indicator
        (if (evil-insert-state-p)
            "Insert"
          "Normal")))

(defun my-display-evil-mode-indicator ()
  (let ((window (get-buffer-window "*Evil Mode Indicator*")))
    (if (not window)
        (with-current-buffer (get-buffer-create "*Evil Mode Indicator*")
          (setq mode-line-format nil)  ; Hide mode line
          (setq window-size 0)          ; Set buffer size to 0
          (insert my-evil-mode-indicator))
      (with-current-buffer "*Evil Mode Indicator*"
        (erase-buffer)
        (insert my-evil-mode-indicator)))))

(add-hook 'post-command-hook 'my-update-evil-mode-indicator)
(add-hook 'post-command-hook 'my-display-evil-mode-indicator)

;; Make the indicator buffer invisible
(setq display-buffer-alist
      '(("*Evil Mode Indicator*"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . -1)
         (window-width . 20) ; Adjust width if needed
         (window-height . 1) ; Height of the buffer
         (no-other-window . t)
         (no-delete-other-windows . t))))

;; for compiling code
;; Function to compile C/C++ code
(defun my-compile-cpp ()
  "Compile the current C++ file."
  (interactive)
  (let ((file (buffer-file-name))
        (default-directory (file-name-directory (buffer-file-name))))
    (if (and file (string-match "\\.cpp\\'" file))
        (compile (format "g++ -o %s %s" (file-name-sans-extension file) file))
      (if (and file (string-match "\\.c\\'" file))
          (compile (format "gcc -o %s %s" (file-name-sans-extension file) file))
        (message "Not a C or C++ file")))))
(global-set-key [f9] 'my-compile-cpp)

;; Keybinding for F1 to open a shell
(global-set-key (kbd "<f1>") 'shell)
;; Enable tab bar mode
(tab-bar-mode 0)

;; Optional: Customize the appearance of the tab bar
(setq tab-bar-new-tab-choice "*scratch*")
;; Disable mouse support
(mouse-avoidance-mode 'none)  ;; Disable mouse avoidance
(setq mouse-wheel-mode nil)    ;; Disable mouse wheel scrolling
(global-set-key [mouse-1] 'ignore)  ;; Disable left mouse click
(global-set-key [mouse-2] 'ignore)  ;; Disable middle mouse click
(global-set-key [mouse-3] 'ignore)  
