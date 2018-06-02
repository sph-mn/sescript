(set jQuery.fn.input-observe
  (l (config)
    (let*
      ( (a this)
        (prev-value (chain val a))
        (js-interval #f)
        (update-f
          (l ()
            (let (value (chain val a))
              (if (not (= value prev-value))
                (set prev-value value)
                (config.f value)))))
        (focus-f (l () (set js-interval (setInterval update_f config.interval)))))
      (a.blur (l () (clearInterval js-interval) (a.off "focus" focus-f))))))
