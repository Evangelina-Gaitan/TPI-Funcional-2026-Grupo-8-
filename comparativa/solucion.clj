;;========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Determinista, sin efectos colaterales)
;; ESTRATEGIA: Evaluación de casos mediante la estructura cond
;; IMPACTO: No destructiva (Inmutable, retorna un vector nuevo)
;;========================================================

(defn transicion [color-actual cambiar-a]
  (cond
    (and (= color-actual :en-rojo) (= cambiar-a :verde))
    [color-actual :cambiar-a-verde]

    (and (= color-actual :en-verde) (= cambiar-a :amarillo))
    [color-actual :cambiar-a-amarillo]

    (and (= color-actual :en-amarillo) (= cambiar-a :rojo))
    [color-actual :cambiar-a-rojo]

    :else
    [color-actual :accion-por-defecto]
  )
)

;;========================================================
;; FUNCION: timer
;; NATURALEZA: Pura (Mapeo directo de tiempo a estado, sin estado global)
;; ESTRATEGIA: Control de flujo condicional con enlace de variables locales (let)
;; IMPACTO: No destructiva (Genera un valor escalar a partir del cálculo de ciclo)
;;========================================================

(defn timer [timestamp]
  (let [tiempo-rojo 90
        tiempo-verde 120
        tiempo-amarillo 6
        tiempo-total (+ tiempo-rojo tiempo-verde tiempo-amarillo)
        posicion-ciclo (mod timestamp tiempo-total)]
    (cond
      (< posicion-ciclo tiempo-rojo) :rojo
      (< posicion-ciclo (+ tiempo-rojo tiempo-verde)) :verde
      :else :amarillo)))

;; PRUEBAS CON KEYWORDS
(println (transicion :en-rojo :verde))   ; Imprime: [:en-rojo :cambiar-a-verde]
(println (timer 100))                     ; Imprime: :verde


;;Compilador de clojure online: https://www.mycompiler.io/es/new/clojure
;;O compilador dado por la catedra: https://tryclojure.org/ 
