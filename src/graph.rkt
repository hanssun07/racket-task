#lang racket/base

(require
    "utils/ann.rkt"
    "utils.rkt"
    threading
    data/gvector data/skip-list data/integer-set)

(:typedef IdIdsRef (GVector (IntegerSet Index)))
(:structdef graph : Graph
    ([idx->id         : (GVector Id)]
     [id->idx         : (MHash Id Index)]
     [base-rels       : GraphBaseRels]
     [structure-cache : GraphStructureCache]))
(:structdef graph/base-rels : GraphBaseRels
    ([id->known-parents  : IdIdsRef]
     [id->known-children : IdIdsRef]
     [id->known-unrel    : IdIdsRef]))
(:structdef graph/structure-cache : GraphStructureCache
    ([id->ancestors   : IdIdsRef]
     [id->descendents : IdIdsRef]
     [id->unrel       : IdIdsRef]
     [id->unknown     : IdIdsRef]))

(struct graph
    (idx->id id->idx base-rels structure-cache))
(struct graph/base-rels
    (id->known-parents
     id->known-children
     id->known-unrel))
(struct graph/structure-cache
    (id->ancestors
     id->descendents
     id->unrel
     id->unknown))

(: make-graph (-> Graph))
(define (make-graph)
    (graph (make-gvector)
           (make-hash)
           (graph/base-rels
            (make-gvector)
            (make-gvector)
            (make-gvector))
           (graph/structure-cache
            (make-gvector)
            (make-gvector)
            (make-gvector)
            (make-gvector))))

(: _id->idx (Graph Id -> Index))
(: _idx->id (Graph Index -> Id))
(define (_id->idx g a) (~> g graph-id->idx (hash-ref _ a)))
(define (_idx->id g i) (~> g graph-idx->id (gvector-ref _ I)))

(: _children-of    (Graph Index -> (IntegerSet Index)))
(: _parents-of     (Graph Index -> (IntegerSet Index)))
(: _descendents-of (Graph Index -> (IntegerSet Index)))
(: _ancestors-of   (Graph Index -> (IntegerSet Index)))
(: _unrelated-to   (Graph Index -> (IntegerSet Index)))
(: _unknown-to     (Graph Index -> (IntegerSet Index)))
(: _rel-of ((Graph -> a) (a -> IdIdsRef) -> (Graph Index -> IntegerSet))
           #:syn-helper)
(define ((_rel-of getter1 getter2) g i)
    (~> (getter1 g)
        (getter2 _)
        (gvector-ref _ i)))
(define _childen-of     (_rel-of graph-base-rels graph/base-rels-id->known-parents))
(define _parents-of     (_rel-of graph-base-rels graph/base-rels-id->known-children))
(define _descendents-of (_rel-of graph-structure-cache graph/structure-cache-id->ancestors))
(define _ancestors-of   (_rel-of graph-structure-cache graph/structure-cache-id->descendents))
(define _unrelated-to   (_rel-of graph-structure-cache graph/structure-cache-id->unrel))
(define _unknown-to     (_rel-of graph-structure-cache graph/structure-cache-id->unknown))

(: _is-head? (Graph Index -> Bool))
(: _is-tail? (Graph Index -> Bool))
(define (_is-head? g i) (~> (_ancestors-of   g i) get-integer not))
(define (_is-tail? g i) (~> (_descendents-of g i) get-integer not))

(: _is-child-of?      (Graph Index Index -> Bool))
(: _is-parent-of?     (Graph Index Index -> Bool))
(: _is-descendent-of? (Graph Index Index -> Bool))
(: _is-ancestor-of?   (Graph Index Index -> Bool))
(: _is-unrelated-to?  (Graph Index Index -> Bool))
(: _is-unknown-to?    (Graph Index Index -> Bool))
(: _is-rel-of? ((Graph -> a) (a -> IdIdsRef) -> (Graph Index Index -> Bool))
               #:syn-helper)
(define ((_is-rel-of? getter1 getter2) ai bi) (member? bi (_rel-of g ai getter1 getter2)))
(define _is-child-of?      (_is-rel-of? graph-base-rels graph/base-rels-id->known-parents))
(define _is-parent-of?     (_is-rel-of? graph-base-rels graph/base-rels-id->known-children))
(define _is-descendent-of? (_is-rel-of? graph-structure-cache graph/structure-cache-id->ancestors))
(define _is-ancestor-of?   (_is-rel-of? graph-structure-cache graph/structure-cache-id->descendents))
(define _is-unrelated-to?  (_is-rel-of? graph-structure-cache graph/structure-cache-id->unrel))
(define _is-unknown-to?    (_is-rel-of? graph-structure-cache graph/structure-cache-id->unknown))

(: graph-is-child-of?       (Graph Id Id -> Bool))
(: graph-is-parent-of?      (Graph Id Id -> Bool))
(: graph-is-descendent-of?  (Graph Id Id -> Bool))
(: graph-is-ancestor-of?    (Graph Id Id -> Bool))
(: graph-is-unrelated-to?   (Graph Id Id -> Bool))
(: graph-is-unknown-to?     (Graph Id Id -> Bool))
(define (graph-is-child-of?      g a b) (_is-child-of?      g (_id->idx g a) (_id->idx g b)))
(define (graph-is-parent-of?     g a b) (_is-parent-of?     g (_id->idx g a) (_id->idx g b)))
(define (graph-is-descendent-of? g a b) (_is-descendent-of? g (_id->idx g a) (_id->idx g b)))
(define (graph-is-ancestor-of?   g a b) (_is-ancestor-of?   g (_id->idx g a) (_id->idx g b)))
(define (graph-is-unrelated-to?  g a b) (_is-unrelated-to?  g (_id->idx g a) (_id->idx g b)))
(define (graph-is-unknown-to?    g a b) (_is-unknown-to?    g (_id->idx g a) (_id->idx g b)))

(: graph-relation-of (Graph Id Id -> (U 'self 'child 'parent
                                        'descendent 'ancestor
                                        'unrelated 'unknown)))
(define (graph-relation-of g a b)
    (define ai (_id->idx g a))
    (define bi (_id->idx g b))
    (cond
        [(eq? ai bi) 'self]
        [(_is-child-of?      g ai bi) 'child]
        [(_is-parent-of?     g ai bi) 'parent]
        [(_is-descendent-of? g ai bi) 'descendent]
        [(_is-ancestor-of?   g ai bi) 'ancestor]
        [(_is-unrelated-to?  g ai bi) 'unrelated]
        [(_is-unknown-to?    g ai bi) 'unknown]))

(: _in-children-of     (Graph Index -> (Sequenceof Index)))
(: _in-parents-of      (Graph Index -> (Sequenceof Index)))
(: _in-ancestors-of    (Graph Index -> (Sequenceof Index)))
(: _in-descendents-of  (Graph Index -> (Sequenceof Index)))
(: _in-unrelated-to    (Graph Index -> (Sequenceof Index)))
(: _in-unknown-to      (Graph Index -> (Sequenceof Index)))
(: _in-rel-of ((Graph Index -> (IntegerSet Index)) -> (Graph Index -> (Sequenceof Index)))
              #:syn-helper)
; IntegerSet is already a Sequence
(define ((_in-rel-of getter) g ai) (getter g ai))
(define _in-children-of    (_in-rel-of _children-of))
(define _in-parents-of     (_in-rel-of _parents-of))
(define _in-descendents-of (_in-rel-of _descendents-of))
(define _in-ancestors-of   (_in-rel-of _ancestors-of))
(define _in-unrelated-to   (_in-rel-of _unrelated-to))
(define _in-unknown-to     (_in-rel-of _unknown-to))

(: _in-heads-of (Graph Index -> (Sequenceof Index)))
(: _in-tails-of (Graph Index -> (Sequenceof Index)))
(define (_in-heads-of g ai) (sequence-filter (lambda~> (_is-head? g _)) (_in-ancestors-of   g ai)))
(define (_in-tails-of g bi) (sequence-filter (lambda~> (_is-tail? g _)) (_in-descendents-of g bi)))

(: in-parents-of      (Graph Id -> (Sequenceof Id)))
(: in-children-of     (Graph Id -> (Sequenceof Id)))
(: in-ancestors-of    (Graph Id -> (Sequenceof Id)))
(: in-descendents-of  (Graph Id -> (Sequenceof Id)))
(: in-heads-of        (Graph Id -> (Sequenceof Id)))
(: in-tails-of        (Graph Id -> (Sequenceof Id)))
(: in-rel-of ((Graph Index -> (Sequenceof Index)) -> (Graph Id -> (Sequenceof Id)))
             #:syn-helper)
(define ((in-rel-of _inner-rel-of) g a)
    (sequence-map (lambda~> (_idx->id g _)) (_inner-rel-of g (_id->idx g a))))
(define in-children-of    (in-rel-of _in-children-of))
(define in-parents-of     (in-rel-of _in-parents-of))
(define in-descendents-of (in-rel-of _in-descendents-of))
(define in-ancestors-of   (in-rel-of _in-ancestors-of))
(define in-heads-of       (in-rel-of _in-heads-of))
(define in-tails-of       (in-rel-of _in-tails-of))

(: _in-nodes (Graph -> (Sequenceof Index)))
(: _in-heads (Graph -> (Sequenceof Index)))
(: _in-tails (Graph -> (Sequenceof Index)))
(define (_in-nodes g) (in-range (gvector-count g)))
(define (_in-heads g) (sequence-filter (lambda~> (_is-head? g _)) (_in-nodes g)))
(define (_in-tails g) (sequence-filter (lambda~> (_is-tail? g _)) (_in-nodes g)))

(: in-graph-nodes (Graph -> (Sequenceof Id)))
(: in-graph-heads (Graph -> (Sequenceof Id)))
(: in-graph-tails (Graph -> (Sequenceof Id)))
(: in-graph-of ((Graph -> (Sequenceof Index)) -> (Graph -> (Sequenceof Id)))
               #:syn-helper)
(define ((in-graph-of _in-graph) g) (sequence-map (lambda~> (_idx->id g _)) (_in-graph g)))
(define in-graph-nodes (in-graph-of _in-nodes))
(define in-graph-heads (in-graph-of _in-heads))
(define in-graph-tails (in-graph-of _in-tails))

(: in-graph-unknown-relations/stable (Graph -> (Sequenceof (Pair Id Id))))
(define (in-graph-unknown-relations/stable g)
    (define cache (~> (graph-structure-cache g) (graph/structure-cache-id->unknown)))
    (define idx->id (graph-idx->id g))
    (in-generator (for* ([ai (gvector-count cache)]
                         [bi (gvector-ref cache ai)])
        (when (and (< ai bi)
                   (member? bi (gvector-ref cache ai)))
           (yield (cons (gvector-ref idx->id ai) (gvector-ref idx->id bi)))))))
                       
(: _propagate-parent-child (Graph Index Index -> Void)
    (:invar "pi is ancestor of ci"))
(: _fill-unrel (Graph Index Index -> Void))
(define (_propagate-parent-child g pi ci)
    (define p-desc (union (_descendents-of g pi)
                          (_descendents-of g ci)))
    (define c-ansc (union (_ancestors-of g pi)
                          (_ancestors-of g ci)))
    (define descendent-cache (~> g graph-structure-cache graph/structure-cache-id->descendents))
    (define ancestor-cache (~> g graph-structure-cache graph/structure-cache-id->ancestors))
    (unless (equal? p-desc (_descendents-of g pi))
        (for ([i (in-graph-node-ancestors g pi)])
            (gvector-set! descendent-cache i
             (union (gvector-ref descendent-cache i) p-desc))))
    (unless (equal? c-ansc (_ancestors-of g ci))
    (for ([i (in-graph-node-descendents g ci)])
        (gvector-set! ancestor-cache i
         (union (gvector-ref ancestor-cache i) c-ansc))))
)
                
            

(: graph-add-node!      (Graph Id    -> (U Void (^ Exn:Fail))))
(: graph-parent-child!  (Graph Id Id -> (U Void (^ Exn:Fail))))
(: graph-unrelated!     (Graph Id Id -> (U Void (^ Exn:Fail))))

