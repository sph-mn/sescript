(library (a test lib)
  (export change_path)
  (import (sph-cms base) (jquery))
  (set! jquery.fn.input-observe
    (l (config)
      (let*
        ( (a this)
          (prev-value (chain val a))
          (js-interval #f)
          (update-func
            (lambda ()
              (let (value (chain val a))
                (if (not (eqv? value prev-value))
                  (set! prev-value value)
                  (config.func value)))))
          (focus-func
            (lambda ()
              (set! js-interval (setInterval update_func config.interval)))))
        (a.blur
          (l ()
            (clearInterval js-interval)
            (a.off "focus" focus-func)))))))
