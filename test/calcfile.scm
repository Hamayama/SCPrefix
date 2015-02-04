#!/usr/bin/env gosh
;; -*- coding: utf-8 -*-
;;
;; calcfile.scm
;; 2015-2-4
;;
;; ＜内容＞
;;   ファイルからS式を読み込んで評価し、
;;   結果をファイルに出力する Gauche のスクリプトです。
;;
;; ＜使い方＞
;;   gosh calcfile.scm file1 file2
;;   を実行すると、file1 からS式を読み込んで評価し、
;;   結果を file2 に出力します。
;;
;; ＜注意事項＞
;;   ファイル内のS式で使用できる命令は、
;;   サンドボックス モジュール内で許可されたもののみです。
;;

;; サンドボックス モジュール生成マクロ
;; ( http://valvallow.blogspot.jp/2011/06/gauchemoduledefine-in-moduleevalsandbox.html )
(define-macro (define-sandbox name modules . symbols)
  `(begin
     (define-module ,name)
     (define ,name (find-module (quote ,name)))
     (eval (quote (begin ,@(map (lambda (m)
                                  `(use ,m))
                                modules))) ,name)
     ;; (完全に空のモジュールにするため変更)
     ;(eval '(extend null) ,name)
     (eval '(extend) ,name)
     (begin
       ,@(map (lambda (sym)
                `(define-in-module ,name ,sym ,sym))
              symbols)
       (undefined))))

;; サンドボックス モジュールの実体を生成
(define-sandbox calcmod ()
  ;; (許可する命令を記述)
  inc! dec! expt not lognot
  * / *. /. quotient modulo
  + - +. -. ash
  < <= > >= =
  logand logior logxor
  and or if
  set! define
  begin begin0
  sin cos gcd
  vector ~)


;; デバッグ表示
(define (debug-print-str str)
  (display str (current-error-port))
  (newline (current-error-port)))

(define (debug-print-sexpr s)
  (write s (current-error-port))
  (newline (current-error-port)))


;; 使い方表示
(define (usage)
  (display 
   "Usage: gosh calcfile.scm file1 file2\n\
    Eval s-expressions in a file1 and output results to a file2.\n"
   (current-error-port))
  (exit 2))


;; 本体
(define (main args)
  (define fname1 "")
  (define fname2 "")

  ;; 引数チェック
  (if (< (length args) 3)
    (usage))

  ;; ファイル名の取得
  (set! fname1 (list-ref args 1))
  (set! fname2 (list-ref args 2))
  (debug-print-str fname1)
  (debug-print-str fname2)

  ;; ファイルからS式を1個ずつ読み込んで評価し、結果をファイルに1行ずつ出力する
  (with-input-from-file fname1
    (lambda ()
      (with-output-to-file fname2
        (lambda ()
          (do ((s (read) (read)))
              ((eof-object? s) #f)
            (debug-print-sexpr s)
            (print (eval s calcmod))
            )))))

  0)

