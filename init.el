
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(ido-mode 1)
(setq default-directory "C:/Users/prem/")

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(electric-pair-mode 1)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7"
     default))
 '(package-selected-packages
   '(evil gruber-darker-theme koopa-mode pdf-tools powershell vterm
	  vterm-hotkey vtm))
 '(warning-suppress-types '((treesit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)

;; Add MELPA repository (where evil is hosted)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; Disable evil in minibuffer
(defun my/disable-evil-in-minibuffer ()
  (evil-local-mode -1))


(add-hook 'minibuffer-setup-hook #'my/disable-evil-in-minibuffer)

(set-face-attribute 'default nil
  :family "Iosevka NFP"
  :height 150)
(setenv "PATH" (concat "C:/Program Files/CMake/bin;" (getenv "PATH")))
(add-to-list 'exec-path "C:/Program Files/CMake/bin")

(setq evil-normal-state-cursor 'box
      evil-insert-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-replace-state-cursor 'box
      evil-motion-state-cursor 'box
      evil-emacs-state-cursor  'box)



(setq tab-stop-list (number-sequence 4 200 4))
;; No backup files (~)
(setq make-backup-files nil)

;; No auto-save files (#foo#)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; No lock files (.#foo)
(setq create-lockfiles nil)

;; Extra safety: disable versioned backups
(setq version-control nil)

(show-paren-mode 1)


(require 'cc-mode)
(defun rc/set-up-whitespace-handling ()
  "Custom whitespace and indentation settings for C/C++."
  (setq indent-tabs-mode nil)     ;; use spaces instead of tabs
  (setq tab-width 4)              ;; set tab width to 4
  (setq c-basic-offset 4))         ;; indentation = 4 spaces
(add-hook 'c-mode-common-hook #'rc/set-up-whitespace-handling)
;; Enable it for C++ (and optionally C)
(add-hook 'c++-mode-hook #'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook #'rc/set-up-whitespace-handling)
;; Always use UTF-8
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Optional: force new files to UTF-8
(setq-default default-buffer-file-coding-system 'utf-8-unix)

(setenv "PATH"
        (concat
         "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.44.35211/bin/Hostx64/x64;"
         "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/CommonExtensions/Microsoft/CMake/CMake/bin;"
         (getenv "PATH")))

(defun eshell/cls ()
  "Clear the eshell buffer, like Windows 'cls'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
