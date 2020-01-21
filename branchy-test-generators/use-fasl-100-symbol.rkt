#lang racket/base

(require racket/fasl)

(define f #"racket/fasl:\0\200C\4\34d\20\bsymbol89\20\bsymbol90\20\bsymbol91\20\bsymbol92\20\bsymbol93\20\bsymbol94\20\bsymbol95\20\bsymbol96\20\bsymbol97\20\bsymbol98\20\bsymbol99\20\tsymbol100\20\tsymbol101\20\tsymbol102\20\tsymbol103\20\tsymbol104\20\tsymbol105\20\tsymbol106\20\tsymbol107\20\tsymbol108\20\tsymbol109\20\tsymbol110\20\tsymbol111\20\tsymbol112\20\tsymbol113\20\tsymbol114\20\tsymbol115\20\tsymbol116\20\tsymbol117\20\tsymbol118\20\tsymbol119\20\tsymbol120\20\tsymbol121\20\tsymbol122\20\tsymbol123\20\tsymbol124\20\tsymbol125\20\tsymbol126\20\tsymbol127\20\tsymbol128\20\tsymbol129\20\tsymbol130\20\tsymbol131\20\tsymbol132\20\tsymbol133\20\tsymbol134\20\tsymbol135\20\tsymbol136\20\tsymbol137\20\tsymbol138\20\tsymbol139\20\tsymbol140\20\tsymbol141\20\tsymbol142\20\tsymbol143\20\tsymbol144\20\tsymbol145\20\tsymbol146\20\tsymbol147\20\tsymbol148\20\tsymbol149\20\tsymbol150\20\tsymbol151\20\tsymbol152\20\tsymbol153\20\tsymbol154\20\tsymbol155\20\tsymbol156\20\tsymbol157\20\tsymbol158\20\tsymbol159\20\tsymbol160\20\tsymbol161\20\tsymbol162\20\tsymbol163\20\tsymbol164\20\tsymbol165\20\tsymbol166\20\tsymbol167\20\tsymbol168\20\tsymbol169\20\tsymbol170\20\tsymbol171\20\tsymbol172\20\tsymbol173\20\tsymbol174\20\tsymbol175\20\tsymbol176\20\tsymbol177\20\tsymbol178\20\tsymbol179\20\tsymbol180\20\tsymbol181\20\tsymbol182\20\tsymbol183\20\tsymbol184\20\tsymbol185\20\tsymbol186\20\tsymbol187\20\tsymbol188")

(printf "---- FASL->S-EXP START---- result : ~a\n" (fasl->s-exp f))
(for ([i (in-range 15)])
  (time (for ([j (in-range 500)])
         (fasl->s-exp f))))
(printf "---- FASL->S-EXP END ----\n")