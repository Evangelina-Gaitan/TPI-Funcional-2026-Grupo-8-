;;Requerimiento 1.
;;========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (No tiene efectos secundarios, siempre retorna la misma salida para las mismas entradas)
;; ESTRATEGIA: Condicional simple y Función Predicado
;; IMPACTO: No destructiva
;;========================================================

(defun transicion (color-actual cambiar-a)
 (cond
   ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))
   
   ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))
   
   ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))
   
   (t (list color-actual 'accion-por-defecto))
  )
)


;;Requerimiento 2.
;;========================================================
;; FUNCION: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional simple
;; IMPACTO: No destructiva
;;========================================================

(defun timer (timestamp)
  (let* ((tiempo-rojo 90)
         (tiempo-amarillo 6)
         (tiempo-verde 120)
         (tiempo-total (+ tiempo-rojo tiempo-amarillo tiempo-verde))
         (posicion-ciclo (mod timestamp tiempo-total)))
    (cond
      ((< posicion-ciclo tiempo-rojo) 'rojo)
      ((< posicion-ciclo (+ tiempo-rojo tiempo-verde)) 'verde)
      (t 'amarillo)
    ))
)

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

; Requerimiento 4a
; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmética 
;; IMPACTO: No destructiva
; ========================================================

(defun duracion-ciclo (tiempo-rojo tiempo-amarillo tiempo-verde)
  (+ tiempo-rojo tiempo-amarillo tiempo-verde)
)

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
     "Ciclo dentro del rango óptimo.")
  )
)

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
     (list 'porcentaje-verde (* (/ verde total) 100.0)))
  )
)

;;////////////////////////////////////////////////////////////////////////////////////////////////
;;----------------------------------------------------------------------
;; ITERACION 2 
;; EXTENSION 1

;;Intermitencia de 3 segundos entre los cambios de luces

;;ROJO (90 s)-> ROJO-INTERMITENTE (3 s) -> VERDE (120 s) -> VERDE-INTERMITENTE (3 s) -> AMARILLO (6 s) -> AMARILLO-INTERMITENTE (3 s) -> ROJO
;Total = 225 segundos  

;;========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (No tiene efectos secundarios, siempre retorna la misma salida para las mismas entradas)
;; ESTRATEGIA: Condicional simple y Función Predicado
;; IMPACTO: No destructiva
;;========================================================

(defun transicion (color-actual cambiar-a)

  (cond

    ((and (eq color-actual 'en-rojo)
          (eq cambiar-a 'rojo-intermitente))
     (list color-actual "cambiar-a-rojo-intermitente"))

    ((and (eq color-actual 'en-rojo-intermitente)
          (eq cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde)
          (eq cambiar-a 'verde-intermitente))
     (list color-actual "cambiar-a-verde-intermitente"))

    ((and (eq color-actual 'en-verde-intermitente)
          (eq cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo)
          (eq cambiar-a 'amarillo-intermitente))
     (list color-actual "cambiar-a-amarillo-intermitente"))

    ((and (eq color-actual 'en-amarillo-intermitente)
          (eq cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))

    (t (list color-actual 'accion-por-defecto))
  )
)

;;========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional simple 
;; IMPACTO: No destructiva
;;========================================================

(defun timer (timestamp)

  (let* ( (intermitencia 3)
          (tiempo-rojo 90)
          (tiempo-verde 120)
          (tiempo-amarillo 6)

          (base (+ tiempo-rojo tiempo-verde tiempo-amarillo))
          (tiempo-total (+ base (* 3 intermitencia)))

          (posicion-ciclo (mod timestamp tiempo-total))
        )

    (cond
      ((< posicion-ciclo tiempo-rojo) 'rojo)
     
      ((< posicion-ciclo (+ tiempo-rojo intermitencia)) 'rojo-intermitente)
      ((< posicion-ciclo (+ tiempo-rojo intermitencia tiempo-verde)) 'verde)
      ((< posicion-ciclo (+ tiempo-rojo intermitencia tiempo-verde intermitencia)) 'verde-intermitente)
      ((< posicion-ciclo (+ tiempo-rojo intermitencia tiempo-verde intermitencia tiempo-amarillo)) 'amarillo)
      (t 'amarillo-intermitente)
    )
  )
)

;;========================================================
;; FUNCIÓN: registrarEstadoLuces
;; NATURALEZA: Impura
;; ESTRATEGIA: Secuencial 
;; IMPACTO: No destructiva
;;========================================================

(defun registrarEstadoLuces (timestamp color-anterior color-nuevo)

  (format t
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          timestamp
          color-anterior
          color-nuevo)

  'registro-guardado)

;;========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmetica 
;; IMPACTO: No destructiva
;;========================================================

(defun duracion-ciclo (tiempo-rojo tiempo-amarillo tiempo-verde)
  (+ tiempo-rojo tiempo-amarillo tiempo-verde 9)
)
;;========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional Simple 
;; IMPACTO: No destructiva
;;========================================================

(defun recomendacion-ciclo (duracion)

  (cond
    ((< duracion 35)
     "Ciclo demasiado corto. Se recomienda aumentarlo.")

    ((> duracion 150)
     "Ciclo demasiado largo. Se recomienda reducirlo.")

    (t
     "Ciclo dentro del rango óptimo.")))

;;========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmetica
;; IMPACTO: No destructiva
;;========================================================

(defun ciclos-por-tiempo (minutos)
  (nth-value 0 (floor (* minutos 60) 225)))

;;========================================================
;; FUNCIÓN: distribucion-porcentual
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmetica
;; IMPACTO: No destructiva
;;========================================================
  
(defun distribucion-porcentual (rojo amarillo verde)

  (let ((total (+ rojo amarillo verde 9)))

    (list

      (list 'porcentaje-rojo
            (* (/ rojo total) 100.0))

      (list 'porcentaje-rojo-intermitente
            (* (/ 3 total) 100.0))

      (list 'porcentaje-amarillo
            (* (/ amarillo total) 100.0))

      (list 'porcentaje-amarillo-intermitente
            (* (/ 3 total) 100.0))

      (list 'porcentaje-verde
            (* (/ verde total) 100.0))

      (list 'porcentaje-verde-intermitente
            (* (/ 3 total) 100.0)))))

;;//////////////////////////////////////////////////////////////////////////////////////
; ========================================================
; EXTENSIÓN 2 - Persistencia de Datos
; ========================================================

;=========================================
; FUNCIÓN: escribir-registros
; NATURALEZA: Impura
; ESTRATEGIA: Recursividad
; IMPACTO: No destructiva
;=========================================

(defun escribir-registros (datos stream)
  (cond
    ((null datos) nil)

    (t
      (format stream
              "A - Transicion: ~A -> ~A%"
              (local-time:format-timestring
               nil
               (local-time:now)
               :format '(:year "-" (:month 2) "-" 
               (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
              (second (first datos))
              (third (first datos))) 

      (escribir-registros
       (rest datos)
       stream))))

;;===========================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura
;; ESTRATEGIA: Secuencial + Recursividad
;; IMPACTO: No destructiva
;;============================================

(defun informe (datos)

  (with-open-file (stream
                   "informe-ejecucion-semaforos.txt"
                   :direction :output
                   :if-exists :supersede)

    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%")

    (escribir-registros datos stream)

    (format stream "~%--- Fin del Informe ---"))

  'informe-generado)

;;(ql:quickload :local-time)   //necesario 
;;(informe '((100 rojo verde) (220 verde amarillo)(226 amarillo rojo)))


;;ACLARACION, El requerimiento 7 esta como consigna en el sistema de Semaforos Inteligentes
;;lo colocamos para el debido control de funcionamiento.
; ========================================================
;;Requerimiento 7 - Casos de prueba, caso de control
; ========================================================
;;Iteracion 1

;; 1) (transicion 'en-rojo 'verde) 
;; 2) (timer 0) (timer 150) (timer 215)
;; 3) (registrarEstadoLuces 123456 'rojo  'verde) (registrarEstadoLuces 987654 'verde 'amarillo) 
;; 4) a-     (duracion-ciclo 90 6 120)       b- (recomendacion-ciclo 100)
;; 5) (ciclos-por-tiempo 18)
;; 6) (distribucion-porcentual 90 6 120)

;;-------------------------------------------------------
;;Iteracion 2 extension 1

;; Requerimiento 1 caso de exito
;; (transicion 'en-rojo 'rojo-intermitente)
;; (transicion 'en-rojo-intermitente 'verde)

;; alternativa, (transicion 'en-rojo 'verde)

;;error, (transicion) o (transicion 123 'verde)

;;requerimiento 2

;; Caso de exito, (timer 89) (timer 222) (timer 93)
;; alternativa, (timer 225) (timer 450)
;; error, (timer 'tres) (timer nil)

;;requerimiento 3
;; Caso de exito, (registrarEstadoLuces 93 'rojo-intermitente 'verde)
;; alternativa, (registrarEstadoLuces "01:30 AM" 'Desconocido 'Rojo)
;; error,  (registrarEstadoLuces 10 'rojo)

;;requerimiento 4
;; Caso de exito, 4a (duracion-ciclo 90 6 120)  4b (recomendacion-ciclo 100) 
;; alternativa, (recomendacion-ciclo 20) o (recomendacion-ciclo (duracion-ciclo 90 6 120))
;; error, (recomendacion-ciclo "cien") o (duracion-ciclo)

;;requerimiento 5
;; caso de exito, (ciclos-por-tiempo 60) o (ciclos-por-tiempo 120)
;; alternativa, (ciclos-por-tiempo 3.75) o (ciclos-por-tiempo 2)
;; error, (ciclos-por-tiempo 'una-hora)

;;requerimiento 6
;; caso de exito, (distribucion-porcentual 90 6 120)
;; alternativa, (distribucion-porcentual 30 5 200) mucho tiempo a determinada luz
;; error, (distribucion-porcentual 0 0 0)

