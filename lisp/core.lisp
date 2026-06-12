;Requerimiento 1.
;========================================================
; FUNCIÓN: transicion
; NATURALEZA: Pura (No tiene efectos secundarios, siempre retorna la misma salida para las mismas entradas)
; ESTRATEGIA: Condicional simple y Función Predicado
; IMPACTO: No destructiva
;========================================================

(defun transicion (color-actual cambiar-a)
  (cond
    
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))
  
    ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))
    
    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))
    
       (t (list color-actual 'accion-por-defecto))))


;Requerimiento 2.
;========================================================
;; FUNCION: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional simple
;; IMPACTO: No destructiva
;========================================================

(defun timer (timestamp)
  (let* ((tiempo-rojo 90)
         (tiempo-amarillo 6)
         (tiempo-verde 120)
         (tiempo-total (+ tiempo-rojo tiempo-amarillo tiempo-verde))
         (posicion-ciclo (mod timestamp tiempo-total)))
    (cond
      ((< posicion-ciclo tiempo-rojo) 'rojo)
      ((< posicion-ciclo (+ tiempo-rojo tiempo-amarillo)) 'amarillo)
      (t 'verde))))

; Requerimiento 3
; ========================================================
;; FUNCIÓN: registrarEstadoLuces
;; NATURALEZA: Impura ( mecanismo de logging )
;; ESTRATEGIA: Secuencial simple
;; IMPACTO: No destructiva
; ========================================================

(defun registrarEstadoLuces (timestamp color-anterior color-nuevo)
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          timestamp
          color-anterior
          color-nuevo)
    'registro-Guardado)  ; NIL

; Requerimiento 4
; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmética 
;; IMPACTO: No destructiva
; ========================================================

(defun duracion-ciclo (tiempo-rojo tiempo-amarillo tiempo-verde)
  (+ tiempo-rojo tiempo-amarillo tiempo-verde))

;Requerimiento 4b.
;========================================================
;; FUNCION: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional simple
;; IMPACTO: No destructiva
;========================================================

(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35)
     "Ciclo demasiado corto. Se recomienda aumentarlo.")

    ((> duracion 150)
     "Ciclo demasiado largo. Se recomienda reducirlo.")

    (t
     "Ciclo dentro del rango óptimo.")))

;; REQUERIMIENTO 5
;;
;; FUNCION: ciclos-por-tiempo
;; NATURALEZA: Pura (Dado un argumento, siempre le corresponde el mismo resultado)
;; ESTRATEGIA: Función aritmética simple / Combinación de operaciones
;; IMPACTO: no destructiva
;;

(defun ciclos-por-tiempo (minutos)
  (nth-value 0 (floor (* minutos 60) 216))
)

 ; Requerimiento 6.
; ========================================================
; FUNCIÓN: distribucion-porcentual
; NATURALEZA: Pura (Siempre devuelve la misma distribución para las mismas reglas de temporización)
; ESTRATEGIA: Aritmética simple
; IMPACTO: No destructiva
; ========================================================

(defun distribucion-porcentual (rojo amarillo verde)
  (let ((total (+ rojo amarillo verde)))
    (list
     (list 'porcentaje-rojo (* (/ rojo total) 100.0))
     (list 'porcentaje-amarillo (* (/ amarillo total) 100.0))
     (list 'porcentaje-verde (* (/ verde total) 100.0)))))
