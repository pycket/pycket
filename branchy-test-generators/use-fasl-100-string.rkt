#lang racket/base

(require racket/fasl)

(define f #"racket/fasl:\0\200N\4\34d\23\tstring189\23\tstring190\23\tstring191\23\tstring192\23\tstring193\23\tstring194\23\tstring195\23\tstring196\23\tstring197\23\tstring198\23\tstring199\23\tstring200\23\tstring201\23\tstring202\23\tstring203\23\tstring204\23\tstring205\23\tstring206\23\tstring207\23\tstring208\23\tstring209\23\tstring210\23\tstring211\23\tstring212\23\tstring213\23\tstring214\23\tstring215\23\tstring216\23\tstring217\23\tstring218\23\tstring219\23\tstring220\23\tstring221\23\tstring222\23\tstring223\23\tstring224\23\tstring225\23\tstring226\23\tstring227\23\tstring228\23\tstring229\23\tstring230\23\tstring231\23\tstring232\23\tstring233\23\tstring234\23\tstring235\23\tstring236\23\tstring237\23\tstring238\23\tstring239\23\tstring240\23\tstring241\23\tstring242\23\tstring243\23\tstring244\23\tstring245\23\tstring246\23\tstring247\23\tstring248\23\tstring249\23\tstring250\23\tstring251\23\tstring252\23\tstring253\23\tstring254\23\tstring255\23\tstring256\23\tstring257\23\tstring258\23\tstring259\23\tstring260\23\tstring261\23\tstring262\23\tstring263\23\tstring264\23\tstring265\23\tstring266\23\tstring267\23\tstring268\23\tstring269\23\tstring270\23\tstring271\23\tstring272\23\tstring273\23\tstring274\23\tstring275\23\tstring276\23\tstring277\23\tstring278\23\tstring279\23\tstring280\23\tstring281\23\tstring282\23\tstring283\23\tstring284\23\tstring285\23\tstring286\23\tstring287\23\tstring288")

(printf "---- FASL->S-EXP START---- result : ~a\n" (fasl->s-exp f))
(for ([i (in-range 15)])
  (time (for ([j (in-range 500)])
         (fasl->s-exp f))))
(printf "---- FASL->S-EXP END ----\n")