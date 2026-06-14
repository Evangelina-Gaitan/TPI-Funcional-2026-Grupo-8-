
;;TEMA- LENGUAJE: CLOJURE

;;========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;;========================================================

(defn transicion [color-actual cambiar-a]
  (cond
    (and (= color-actual "en-rojo") (= cambiar-a "verde"))
    [color-actual "cambiar-a-verde"]

    (and (= color-actual "en-verde") (= cambiar-a "amarillo"))
    [color-actual "cambiar-a-amarillo"]

    (and (= color-actual "en-amarillo") (= cambiar-a "rojo"))
    [color-actual "cambiar-a-rojo"]

    :else
    [color-actual "accion-por-defecto"]
  )
)

;;========================================================
;; FUNCION: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional simple
;; IMPACTO: No destructiva
;;========================================================

(defn timer [timestamp]
  (let [tiempo-rojo 90
        tiempo-verde 120
        tiempo-amarillo 6
        tiempo-total (+ tiempo-rojo tiempo-verde tiempo-amarillo)
        posicion-ciclo (mod timestamp tiempo-total)]
    (cond
      (< posicion-ciclo tiempo-rojo) "rojo"
      (< posicion-ciclo (+ tiempo-rojo tiempo-verde)) "verde"
      :else "amarillo")
   )
)

;;Compilador de clojure online: https://www.mycompiler.io/es/new/clojure
;;O compilador dado por la catedra: https://tryclojure.org/ 
