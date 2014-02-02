;;; -*- coding: utf-8; lexical-binding  t -*-

(require 'dash)
(require 'mac-low)
(require 'ert-expectations)

(dont-compile
  (when (fboundp 'expectations)
    (desc "アナフォリックマクロの確認")
    (expect '(1 2 3)
      (maclow:aif '(1 2 3)
          it))
    (expect nil
      (maclow:aif (not '(1 2 3))
          it))
    (expect 1
      (maclow:aif '(1 2 3)
          (car it)))
    (expect t
      (maclow:aif '(1 2 3)
          t))
    (expect 2
      (maclow:aif nil
          1
        2))
    (expect '(2 4 6)
      (maclow:awhen '(1 2 3)
        (mapcar #'(lambda (x) (* 2 x)) it)))

    (desc "渡した値は一度だけ評価される")
    (expect 9
      (let* ((x 0)
             (f (lambda (y)
                  (setq x (1+ x))
                  (* x y))))
        (maclow:awhen (funcall f 3)
          (* it it))))

    (desc "g!がプレフィックスのシンボルであるか")
    (expect t
      (maclow:%g!-symbol-p 'g!sym))
    (expect nil
      (-all? #'maclow:%g!-symbol-p '(g1sym o!sym normal-symbol anysymbol)))

    (desc "o!がプレフィックスのシンボルであるか")
    (expect t
      (maclow:%o!-symbol-p 'o!sym))
    (expect nil
      (-all? #'maclow:%o!-symbol-p '(g1sym g!sym normal-symbol anysymbol)))

    (desc "g!がプレフィックスのシンボルのみをuninternなシンボルにする")
    (expect '(1 4 6)
      (maclow:%defmacro!g tmp (b)
        (if nil
            g!a
          b))
      (tmp '(1 4 6)))

    (desc "o!がプレフィックスのシンボルをg!がプレフィックスのシンボルに変換する")
    (expect '(g!sym nosym osym)
      (mapcar #'maclow:%o!-symbol-to-g!-symbol
              '(o!sym nosym osym)))

    (desc "Common Lispのdefmacro!をエミュレートする")
    (expect 4
      (maclow:defmacro! tmp (o!a)
        `(* ,g!a ,g!a))
      (tmp (1+ 1)))
    (expect '(4 2)
      (let ((num 1))
        (maclow:defmacro! tmp (o!a)
          `(* ,g!a ,g!a))
        (let ((r (tmp (cl-incf num))))
          (list r num))))
    ))

(ert-run-tests-batch-and-exit)
