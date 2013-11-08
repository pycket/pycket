(let countdown ([n 1000000000]) (if (< n 0) 1 (countdown (- n 1))))
