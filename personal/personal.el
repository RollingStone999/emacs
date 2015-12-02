;;; personal.el --- Personal configuration entry point.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; Code:

;;; cscope setting begin
;;(add-hook 'c-mode-common-hook 'cscope-minor-mode)
(add-hook 'c-mode-common-hook 'helm-cscope-mode)
;;(define-key global-map [(control f3)]  'cscope-set-initial-directory)
;;(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map (kbd "C-c c s")  'helm-cscope-find-this-symbol)
(define-key global-map (kbd "C-c c g")  'helm-cscope-find-global-definition)
(define-key global-map (kbd "C-c c C")  'helm-cscope-find-called-functions)
(define-key global-map (kbd "C-c c c")  'helm-cscope-find-functions-calling-this-function)
;;; cscope setting end

;;; erc-mode setting begin
(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE"))
;;; erc-mode setting end

;;; gui display setting begin
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
            (scroll-bar-mode -1)))
;;; gui display setting end

;;; c-mode setting begin
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))

;; hs-minor-mode hooks setting begin
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
;; hs-minor-mode hooks setting end

;;; c-mode setting end

;;; tab setting begin
;;(setq-default indent-tabs-mode nil)
(setq default-tab-width 8)
;;; tab setting end

;;; bash-mode setting begin
(setq sh-basic-offset 8)
(setq sh-indentation 8)
;;; bash-mode setting end

;;; magit setting begin
(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
;;; magit setting end

;;; switch-window setting begin
(global-set-key (kbd "C-x o") 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
;;; switch-window setting end

;;; sdcv setting begin
;; rely on external tool sdcv
(global-set-key (kbd "C-x t") 'yasdcv-translate-at-point)
;;; sdcv setting end

;;; newsticker setting begin
(setq newsticker-url-list
      '(("Washington Post" "http://feeds.washingtonpost.com/rss/world")
        ("cnBeta.COM" "http://rss.cnbeta.com/rss")
        ("BBC World" "http://feeds.bbci.co.uk/news/world/rss.xml")
        ("ZhiHu" "http://www.zhihu.com/rss")
        ("osChina" "http://www.oschina.net/news/rss")
        ("SongShuHui" "http://songshuhui.net/feed")
        ("WaitButWhy" "http://waitbutwhy.com/feed")
        ("ScienceDaily" "http://www.sciencedaily.com/rss")
        ("PAMI" "http://csdl.computer.org/rss/tpami.xml")
        ("Knowledge Mining" "http://csdl.computer.org/rss/tkde.xml")
        ("Learning Technologies" "http://csdl.computer.org/rss/tlt.xml")))
(setq newsticker-html-renderer 'w3m-region)
;;; newsticker setting end

;;; smart-mode-line setting begin
(setq sml/theme 'respectful)
(sml/setup)
;;; smart-mode-line setting end

;;; multiple-cursors setting begin
(global-set-key (kbd "C-c c e") 'mc/edit-lines)

(global-set-key (kbd "C-c c > a") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c c > w") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-c c > s") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-c c < a") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c c < w") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c c < s") 'mc/mark-previous-symbol-like-this)

(global-set-key (kbd "C-c c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c c m w") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-c c m s") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-c c m t") 'mc/mark-all-dwim) ;t means try

(global-set-key (kbd "C-c c m d a") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-c c m d w") 'mc/mark-all-words-like-this-in-defun)
(global-set-key (kbd "C-c c m d s") 'mc/mark-all-symbols-like-this-in-defun)

(global-set-key (kbd "C-c c u n") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c c u p") 'mc/unmark-previous-like-this)
;;; multiple-cursors setting end

;;; yasnippet setting begin
(yas-global-mode 1)
;;; yasnippet setting end

;;; toggle files setting begin
(global-set-key (kbd "C-x x") 'ff-find-other-file)
;;; toggle files setting end

;;; ruby seting begin
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;;; ruby seting end

;;; for macosx dash setting begin
(global-unset-key (kbd "C-c h s"))
(global-set-key (kbd "C-c h d") 'dash-at-point)
(global-set-key (kbd "C-c h s") 'dash-at-point-with-docset)
;;; for macosx dash setting end

;;; frame zoom function setting begin
(global-unset-key (kbd "C-x C-+"))
(global-unset-key (kbd "C-x C--"))
(global-set-key (kbd "C-x C-+") 'zoom-frm-in)
(global-set-key (kbd "C-x C--") 'zoom-frm-out)
;;; frame zoom function setting end

;;; helm setting begin
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ;list actions using C-z
;;; helm setting end

;;; theme setting begin
(disable-theme 'zenburn)
(load-theme 'solarized-light)
;;; theme setting end

;;; whcih key setting begin
(which-key-mode 1)
(which-key-setup-side-window-right)
;;; whcih key setting end

;;; fancy-narrow-mode setting begin
(fancy-narrow-mode 1)
;;; fancy-narrow-mode setting end

(provide 'personal)
;;; personal.el ends here
