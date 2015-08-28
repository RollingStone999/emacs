;;; personal.el --- Personal configuration entry point.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; Code:

;;; cscope setting begin
(require 'xcscope)
(add-hook 'c-mode-common-hook 'cscope-minor-mode)
;;(define-key global-map [(control f3)]  'cscope-set-initial-directory)
;;(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map (kbd "C-c c s")  'cscope-find-this-symbol)
(define-key global-map (kbd "C-c c g")  'cscope-find-global-definition)
(define-key global-map (kbd "C-c c C")  'cscope-find-called-functions)
(define-key global-map (kbd "C-c c c")  'cscope-find-functions-calling-this-function)
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
(setq-default indent-tabs-mode nil)
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

;;; multi-occur setting begin
(global-set-key (kbd "C-x c e") 'mc/edit-lines)
(global-set-key (kbd "C-x c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x c a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x c n") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-x c p") 'mc/unmark-previous-like-this)
;;; multi-occur setting end

;;; yasnippet setting begin
(require 'yasnippet)
(yas-global-mode 1)
;;; yasnippet setting end

;;; toggle files setting begin
(global-set-key (kbd "C-x x") 'ff-find-other-file)
;;; toggle files setting end

;;; ruby seting begin
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;;; ruby seting end

;;; for macosx c-space setting begin
(global-unset-key (kbd "C-m"))
(global-set-key (kbd "C-m") 'set-mark-command)
;;; for macosx c-space setting end

;;; for macosx dash setting begin
(global-unset-key (kbd "C-c h s"))
(global-set-key (kbd "C-c h d") 'dash-at-point)
(global-set-key (kbd "C-c h s") 'dash-at-point-with-docset)
;;; for macosx dash setting end

(provide 'personal)
;;; personal.el ends here
