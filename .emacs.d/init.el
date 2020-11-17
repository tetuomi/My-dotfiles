;;x-1 ショートカット

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

(setq inhibit-splash-screen t)

;;色設定
(load-theme 'tango-dark t)
;;(load-theme 'tsdh-dark t)
;;(load-theme 'light-blue t)

;;行・列番号表示
(require 'linum)
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "grey76"
                    :height 0.9)
(column-number-mode t)

;;日本語フォント設定
(set-fontset-font t 'japanese-jisx0208' "TakaoPGothic")
(add-to-list 'face-font-rescale-alist '(".*Takao P.*" . 0.70))

;;行間
(setq-default line-spacing 8)

;;ツールバー非表示
(tool-bar-mode -1)

;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages
   '(company-tabnine company neotree ## yaml-mode web-mode package-utils golden-ratio coffee-mode))
 '(send-mail-function 'mailclient-send-it)
 '(tab-width 4))

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(1.0))

;; エラー音をならなくする
(setq ring-bell-function 'ignore)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; モードラインに行番号表示
(line-number-mode t)

;; 折り返しをしない
;; (setq-default truncate-lines t)
;; (setq-default truncate-partial-width-windows t)

;;file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))

;; 対応する括弧を光らせる
(show-paren-mode 1)

;;バックアップファイル・オートセーブファイル場所指定
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t))
)

;; ediff config
;; ediff 時にフレームを使わない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; load-path で ~/.emacs.d とか書かなくてよくなる
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defun my-all-cc-mode-init ()
   ;; C 系(cc-mode を継承した)モード共通の設定を記述
 
   ;; 空白などを一度に削除
   ;;(c-toggle-hungry-state 1)
 
   ;; 改行時などで自動インデント
   ;;(c-toggle-electric-state 1)
   ;; 
   ;; ";", "}" などを入力したときに自動改行
   ;; 自動インデントも一緒に ON になる

 ;; 行頭 kill-line (C-k) で行全体をカット
 (setq kill-whole-line t)
 (c-set-style "stroustrup")                  ;;スタイルはストラウストラップ
)

(add-hook 'c-mode-common-hook 'my-all-cc-mode-init)
(add-hook 'c++-mode-common-hook 'my-all-cc-mode-init)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;コピー
(define-key global-map(kbd "C-c c")'copy-region-as-kill)
;;ペースト
(define-key global-map (kbd "C-v") 'yank)
;;一個前のペースト(ペーストの後にのみ有効)
(define-key global-map (kbd "C-y") 'yank-pop)

;; クリップボードへのコピー
(setq x-select-enable-clipboard t)

(require 'xclip)
(xclip-mode 1)

;;(when (and (require 'python nil t) (require 'elpy nil t))
;;  (elpy-enable))

;;(when (require 'set-pyenv-version-path nil t)
;; (add-to-list 'exec-path "~/.pyenv/shims"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;current directory 表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
    (cons '(:eval (concat " ("
            (abbreviate-file-name default-directory)
            ")"))
            (cdr ls))))

(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 3) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;; yasnippetとの連携
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
;;(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
;; すぐに完了をトリガーします。
(setq company-idle-delay 0) ;;候補者に番号を付けます（M-1、M-2などを使用して完了を選択します）。
(setq company-show-numbers t)
