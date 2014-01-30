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

    (desc "g!がプレフィックスのシンボル以外をgensymにする")
    (expect '(1 4 6)
      (maclow:%defmacro!g tmp (g!a)
          (if t
              g!a))
      (tmp '(1 4 6)))
    ))

  (ert-run-tests-batch-and-exit)
