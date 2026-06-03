; TRABALHO - AGENDA TELEFÔNICA EM LISP

; Função que retorna os telefones de um contato
; Se o contato não existir, retorna INEXISTENTE
(defun telefones (agenda nome)

  (cond

    ((null agenda)
     'INEXISTENTE)

    ((eq (car (car agenda)) nome)
     (cdr (car agenda)))

    (t
     (telefones
      (cdr agenda)
      nome))
  )
)

; Função para incluir um telefone na agenda
; Se o contato já existir, adiciona o telefone
; Se não existir, cria um novo contato
(defun incluir (agenda contato)

  (cond

    ((null agenda)
     (list contato))

    ((eq (car contato)
         (car (car agenda)))

     (cons

      (cons
       (car contato)

       (append
        (cdr (car agenda))
        (cdr contato)))

      (cdr agenda)))

    (t

     (cons
      (car agenda)

      (incluir
       (cdr agenda)
       contato)))
  )
)

; Remove um telefone de uma lista de telefones
(defun remove-tel (lista tel)

  (cond

    ((null lista)
     nil)

    ((eq (car lista) tel)
     (cdr lista))

    (t

     (cons
      (car lista)

      (remove-tel
       (cdr lista)
       tel)))
  )
)

; Remove um telefone da agenda
; Se o contato ficar sem telefones,
; remove também o contato da agenda
(defun excluir (agenda contato)

  (cond

    ((null agenda)
     nil)

    ((eq (car contato)
         (car (car agenda)))

     (cond

      ((null

        (remove-tel
         (cdr (car agenda))
         (cadr contato)))

       (cdr agenda))

      (t

       (cons

        (cons
         (car (car agenda))

         (remove-tel
          (cdr (car agenda))
          (cadr contato)))

        (cdr agenda)))
      )
     )

    (t

     (cons
      (car agenda)

      (excluir
       (cdr agenda)
       contato)))
  )
)

; TESTES
(defparameter AGENDA nil)

(setq AGENDA
      (incluir
       AGENDA
       '(Bel 32338778)))

(setq AGENDA
      (incluir
       AGENDA
       '(Rose 32666556)))

(setq AGENDA
      (incluir
       AGENDA
       '(Rose 991919191)))

(setq AGENDA
      (incluir
       AGENDA
       '(Beto 32529119)))

(print AGENDA)

; Contato inexistente
(print (telefones AGENDA 'Jose))

; Contato existente
(print (telefones AGENDA 'Rose))

; Remove um telefone de Rose
(setq AGENDA
      (excluir
       AGENDA
       '(Rose 991919191)))

(print (telefones AGENDA 'Rose))

; Remove o último telefone de Rose
(setq AGENDA
      (excluir
       AGENDA
       '(Rose 32666556)))

(print (telefones AGENDA 'Rose))
