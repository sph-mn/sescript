(library (a test lib)
  (export change_path)
  (import sph_cms.base jquery)
  (set! jquery.fn.input-observe
    (l (config)
      (let*
        ( (ele this)
          (prev-value (chain val ele))
          (js-interval #f)
          (update-func
            (lambda ()
              (let (value (chain val ele))
                (if (not (eqv? value prev-value))
                  (set! prev-value value)
                  (config.func value)))))
          (focus-func
            (lambda ()
              (set! js-interval (setInterval update_func config.interval)))))
        (ele.blur
          (l ()
            (clearInterval js-interval)
            (ele.off "focus" focus-func)))))))