#lang racket/base

(require
    "utils/ann.rkt"
    "utils.rkt"
    threading
    data/gvector
    (rename-in data/integer-set
        [count count/integer-set]
        [partition partition/integer-set]
        [foldr foldr/integer-set])
    racket/sequence racket/generator
    racket/list)
(provide
    make-graph
    graph-relation-of
        graph-is-child-of?      graph-is-parent-of?
        graph-is-descendent-of? graph-is-ancestor-of?
        graph-is-unrelated-to?  graph-is-unknown-to?
    in-graph-nodes
        in-graph-heads  in-graph-tails
    in-parents-of   in-children-of
        in-ancestors-of in-descendents-of
        in-heads-of     in-tails-of
    in-subgraph-heads   in-subgraph-tails
    in-graph-chain-between
    graph-oldest-common-descendent-of
    graph-youngest-common-ancestor-of
    graph-add-node! graph-parent-child! graph-unrelated!)

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

(: _idx->id (Graph Index -> Id))
(define (_id->idx g a) (~> g graph-id->idx (hash-ref _ a)))
(define (_idx->id g i) (~> g graph-idx->id (gvector-ref _ i)))

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
(define _children-of    (_rel-of graph-base-rels graph/base-rels-id->known-children))
(define _parents-of     (_rel-of graph-base-rels graph/base-rels-id->known-parents))
(define _base-unrel-to  (_rel-of graph-base-rels graph/base-rels-id->known-unrel))
(define _descendents-of (_rel-of graph-structure-cache graph/structure-cache-id->descendents))
(define _ancestors-of   (_rel-of graph-structure-cache graph/structure-cache-id->ancestors))
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
(define ((_is-rel-of? getter1 getter2) g ai bi) (member? bi ((_rel-of getter1 getter2) g ai)))
(define _is-child-of?      (_is-rel-of? graph-base-rels graph/base-rels-id->known-children))
(define _is-parent-of?     (_is-rel-of? graph-base-rels graph/base-rels-id->known-parents))
(define _is-descendent-of? (_is-rel-of? graph-structure-cache graph/structure-cache-id->descendents))
(define _is-ancestor-of?   (_is-rel-of? graph-structure-cache graph/structure-cache-id->ancestors))
(define _is-unrelated-to?  (_is-rel-of? graph-structure-cache graph/structure-cache-id->unrel))
(define _is-unknown-to?    (_is-rel-of? graph-structure-cache graph/structure-cache-id->unknown))

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

(: _in-nodes (Graph -> (Sequenceof Index)))
(: _in-heads (Graph -> (Sequenceof Index)))
(: _in-tails (Graph -> (Sequenceof Index)))
(define (_in-nodes g) (~> g graph-idx->id gvector-count in-range))
(define (_in-heads g) (sequence-filter (lambda~> (_is-head? g _)) (_in-nodes g)))
(define (_in-tails g) (sequence-filter (lambda~> (_is-tail? g _)) (_in-nodes g)))

(: _update-cache! ((GVector Row) Index (Row Row -> Row) Row -> Void)
                  (: Row (IntegerSet Index)))
(define (_update-cache! cache i op operand)
    (gvector-set! cache i (op (gvector-ref cache i) operand)))
                       
(: _propagate-parent-child (Graph Index Index -> Void)
    (:invar "pi is ancestor of ci"))
#;(: _fill-unrel (Graph Index Index Index -> Void)
    (:invar "ai ancestor of di; ai, di unrel to ci"))
(define (_propagate-parent-child g pi ci)
    (define p-desc (union (_descendents-of g pi)
                          (_descendents-of g ci)))
    (define c-ansc (union (_ancestors-of g pi)
                          (_ancestors-of g ci)))
    (define descendent-cache (~> g graph-structure-cache graph/structure-cache-id->descendents))
    (define ancestor-cache   (~> g graph-structure-cache graph/structure-cache-id->ancestors))
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (for ([ai c-ansc]);ancs-to-update])
        (_update-cache! descendent-cache ai union    p-desc)
        ; no-op (_update-cache! ancestor-cache   ai subtract p-desc)
        (_update-cache! unrel-cache      ai subtract p-desc)
        (_update-cache! unknown-cache    ai subtract p-desc))
    (for ([di p-desc]);decs-to-update])
        ; no-op (_update-cache! descendent-cache di subtract c-ansc)
        (_update-cache! ancestor-cache   di union    c-ansc)
        (_update-cache! unrel-cache      di subtract c-ansc)
        (_update-cache! unknown-cache    di subtract c-ansc)))
#;(define (_fill-unrel g ai di ci)
    (define bis (_chain-between g ai di))
    (define to-unrel (intersect bis (_unknown-to ci)))
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (_update-cache! unrel-cache   ci union    to-unrel)
    (_update-cache! unknown-cache ci subtract to-unrel)
    (for ([i to-unrel])
        (_update-cache! unrel-cache   i union    (make-range ci))
        (_update-cache! unknown-cache i subtract (make-range ci))))

; this should be unnecessary: -> can override --,
; but after propagation all unaffected -- are still valid
; and all flipped -- are correct
; TODO: see if this can be proved
#;(define (_recalc-unrel g)
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (for ([i (_in-nodes g)])
        (define unrels (_unrelated-to g i))
        (define base-unrels (_base-unrel-to g i))
        (_update-cache! unknown-cache i union     unrels)
        (_update-cache! unrel-cache   i intersect base-unrels))
    (_propagate-unrel g))

(: _propagate-unrel (Graph -> Void))
(define (_propagate-unrel g)
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (define (one-node i)
        (define unrels (_unrelated-to g i))
        (define heads (_heads-within g unrels))
        (define tails (_tails-within g unrels))
        (define within (foldl union (make-range)
            (for*/list ([hi heads] [ti (intersect tails (_descendents-of g hi))])
                (_chain-between g hi ti))))
        (define to-unrel (subtract within unrels))
        (and (get-integer to-unrel)
        (_update-cache! unrel-cache   i union    to-unrel)
        (_update-cache! unknown-cache i subtract to-unrel)
        (for ([j to-unrel])
            (_update-cache! unrel-cache   j union    (make-range i))
            (_update-cache! unknown-cache j subtract (make-range i)))
        #t))
    (define (one-pass) (ormap values (for/list ([i (_in-nodes g)]) (one-node i))))
    (for/and ([_ (in-naturals)]) (one-pass))
    (void))

(: _heads-within (Graph (IntegerSet Index) -> (IntegerSet Index)))
(: _tails-within (Graph (IntegerSet Index) -> (IntegerSet Index)))
(: _oldest-common-descendent (Graph Index Index -> (? Index)))
(: _youngest-common-ancestor (Graph Index Index -> (? Index)))
(: _chain-between (Graph Index Index -> (IntegerSet Index)))
(define (_heads-within g iset)
    (foldr/integer-set (lambda (i set)
        (if (get-integer (intersect iset (_ancestors-of g i)))
            set (union (make-range i) set)))
        (make-range) iset))
(define (_tails-within g iset)
    (foldr/integer-set (lambda (i set)
        (if (get-integer (intersect iset (_descendents-of g i)))
            set (union (make-range i) set)))
        (make-range) iset))
(define (_oldest-common-descendent g ai bi)
    (define ai-descs (_descendents-of g ai))
    (define bi-descs (_descendents-of g bi))
    (define common (intersect ai-descs bi-descs))
    (and (get-integer common)
         (argmax (lambda~> (_descendents-of g _) count/integer-set) common)))
(define (_youngest-common-ancestor g ai bi)
    (define ai-ancs (_ancestors-of g ai))
    (define bi-ancs (_ancestors-of g bi))
    (define common (intersect ai-ancs bi-ancs))
    (and (get-integer common)
         (argmax (lambda~> (_ancestors-of g _) count/integer-set) common)))
(define (_chain-between g ai di)
    (define ai-descs (_descendents-of g ai))
    (define di-ancs  (_ancestors-of   g di))
    (intersect ai-descs di-ancs))

; validates invariants
(define (__validate-graph g)
    (define descendent-cache (~> g graph-structure-cache graph/structure-cache-id->descendents))
    (define ancestor-cache   (~> g graph-structure-cache graph/structure-cache-id->ancestors))
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (define child-known  (~> g graph-base-rels graph/base-rels-id->known-children))
    (define parent-known (~> g graph-base-rels graph/base-rels-id->known-parents))
    (define unrel-known  (~> g graph-base-rels graph/base-rels-id->known-unrel))
    (define id->idx (~> g graph-id->idx))
    (define idx->id (~> g graph-idx->id))
    ; id->idx and idx->id are bijections
    (assert!! (= (gvector-count idx->id) (hash-count id->idx)))
    (for ([i (gvector-count idx->id)])
        (expect!! (= i (_id->idx g (_idx->id g i)))
            "#~a -> ~a -> #~a fails to be a bijection"
            i (_idx->id g i) (_id->idx g (_idx->id g i))))
    ; all knowns/caches have n elems
    ; all known/cache rows are subset of [0, n)
    (define n (gvector-count idx->id))
    (for ([c (list descendent-cache ancestor-cache unrel-cache unknown-cache
                   child-known parent-known unrel-known)])
        (assert!! (= n (gvector-count c)))
        (for ([i n])
            (assert!! (subset? (gvector-ref c i) (make-range 0 (sub1 n))))))
    ; all known -> implies cached ->
    ; all known <- implies cached <-
    ; all known -- implies cached --
    (for ([i n])
        (assert!! (subset? (gvector-ref  child-known i) (gvector-ref descendent-cache i)))
        (assert!! (subset? (gvector-ref parent-known i) (gvector-ref   ancestor-cache i)))
        (assert!! (subset? (gvector-ref  unrel-known i) (gvector-ref      unrel-cache i))))
    ; in cache: all i, j is exactly one of i = j, i -> j, i <- j, i -- j, i -? j
    ; in cache: i -> j implies j <- i
    ; in cache: i <- j implies j -> i
    ; in cache: i -- j implies j -- i
    ; in cache: i -? j implies j -? i
    (define (onee? ?s) (= 1 (count values ?s)))
    (for* ([i n] [j n])
        (assert!! (onee? (list (= i j) (_is-descendent-of? g i j) (_is-ancestor-of? g i j)
                               (_is-unrelated-to? g i j) (_is-unknown-to? g i j))))
        (when (_is-descendent-of? g i j) (assert!!   (_is-ancestor-of? g j i)))
        (when   (_is-ancestor-of? g i j) (assert!! (_is-descendent-of? g j i)))
        (when  (_is-unrelated-to? g i j) (assert!!  (_is-unrelated-to? g j i)))
        (when    (_is-unknown-to? g i j) (assert!!    (_is-unknown-to? g j i))))
    ; in cache: a -> b -> c implies a -> c
    ; in cache: a -> b -> c, a -- d -- c implies b -- d
    (for* ([ai n] [bi (_descendents-of g ai)] [ci (_descendents-of g bi)])
        (expect!! (_is-descendent-of? g ai ci)
            "#~a -> #~a -> #~a, expected #~a -> #~a" ai bi ci ai ci)
        (for ([di (intersect (_unrelated-to g ai) (_unrelated-to g ci))])
            (assert!! (_is-unrelated-to? g bi di)))))

(define (__dump-graph g)
    (for ([i (_in-nodes g)])
        (define id (_idx->id g i))
        (for ([j (_in-nodes g)])
            (define jd (_idx->id g j))
            (when (_is-descendent-of? g i j) (printf "(~a -> ~a) " id jd))
            (when   (_is-ancestor-of? g i j) (printf "(~a <- ~a) " id jd))
            (when  (_is-unrelated-to? g i j) (printf "(~a -- ~a) " id jd)))
        (newline)))

(module+ main
    (define g (make-graph))
    (displayln "adding chain")
    (graph-add-node! g 'chain0)
    (graph-add-node! g 'chain1)
    (graph-add-node! g 'chain2)
    (graph-add-node! g 'chain3)
    (__validate-graph g)
    (__dump-graph g)
    (displayln "chaining chain")
    (graph-parent-child! g 'chain0 'chain1)
    (graph-parent-child! g 'chain1 'chain2)
    (graph-parent-child! g 'chain2 'chain3)
    (__dump-graph g)
    (__validate-graph g)
    (displayln "adding, chaining bar")
    (graph-add-node! g 'bar0)
    (graph-add-node! g 'bar1)
    (graph-add-node! g 'bar2)
    (graph-add-node! g 'bar3)
    (graph-parent-child! g 'bar0 'bar1)
    (graph-parent-child! g 'bar1 'bar2)
    (graph-parent-child! g 'bar2 'bar3)
    (__validate-graph g)
    (__dump-graph g)
    (displayln "half unrel")
    (graph-unrelated! g 'bar0 'chain0)
    (graph-unrelated! g 'bar3 'chain0)
    (__validate-graph g)
    (__dump-graph g)
    (displayln "full unrel")
    (graph-unrelated! g 'bar0 'chain3)
    (graph-unrelated! g 'bar3 'chain3)
    (__validate-graph g)
    (__dump-graph g)
)

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

(: in-graph-nodes (Graph -> (Sequenceof Id)))
(: in-graph-heads (Graph -> (Sequenceof Id)))
(: in-graph-tails (Graph -> (Sequenceof Id)))
(: in-graph-of ((Graph -> (Sequenceof Index)) -> (Graph -> (Sequenceof Id)))
               #:syn-helper)
(define ((in-graph-of _in-graph) g) (sequence-map (lambda~> (_idx->id g _)) (_in-graph g)))
(define in-graph-nodes (in-graph-of _in-nodes))
(define in-graph-heads (in-graph-of _in-heads))
(define in-graph-tails (in-graph-of _in-tails))

(: in-subgraph-heads (Graph (Sequenceof Id) -> (Sequenceof Id)))
(: in-subgraph-tails (Graph (Sequenceof Id) -> (Sequenceof Id)))
(: in-graph-chain-between (Graph Id Id -> (Sequenceof Id)))
(define (in-subgraph-heads g ids)
    (define idxs (foldl union (make-range) (map (lambda~> (_id->idx g _)) ids)))
    (sequence-map (lambda~> (_idx->id g _)) (_heads-within g idxs)))
(define (in-subgraph-tails g ids)
    (define idxs (foldl union (make-range) (map (lambda~> (_id->idx g _)) ids)))
    (sequence-map (lambda~> (_idx->id g _)) (_tails-within g idxs)))
(define (in-graph-chain-between g a b)
    (sequence-map (lambda~> (_idx->id g _)) (_chain-between g (_id->idx g a) (_id->idx g b))))

(: graph-oldest-common-descendent-of (Graph Id Id -> (? Id)))
(: graph-youngest-common-acnestor-of (Graph Id Id -> (? Id)))
(define (graph-oldest-common-descendent-of g a b)
    (_idx->id g (_oldest-common-descendent g (_id->idx a) (_id->idx b))))
(define (graph-youngest-common-ancestor-of g a b)
    (_idx->id g (_youngest-common-ancestor g (_id->idx a) (_id->idx b))))

(: in-graph-unknown-relations/stable (Graph -> (Sequenceof (Pair Id Id))))
(define (in-graph-unknown-relations/stable g)
    (define cache (~> (graph-structure-cache g) (graph/structure-cache-id->unknown)))
    (define idx->id (graph-idx->id g))
    (in-generator (for* ([ai (gvector-count cache)]
                         [bi (gvector-ref cache ai)])
        (when (and (< ai bi)
                   (member? bi (gvector-ref cache ai)))
           (yield (cons (gvector-ref idx->id ai) (gvector-ref idx->id bi)))))))
    
(: graph-add-node!      (Graph Id    -> (U Void (^ Exn:Fail))))
(: graph-parent-child!  (Graph Id Id -> (U Void (^ Exn:Fail))))
(: graph-unrelated!     (Graph Id Id -> (U Void (^ Exn:Fail))))
(define (graph-add-node! g id)
    (define i (~> g graph-idx->id gvector-count))
    (define 0:..i (if (zero? i) (make-range) (make-range 0 (sub1 i))))
    (define empty/range (make-range))
    (define unknown-cache (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (for ([j (_in-nodes g)])
        (_update-cache! unknown-cache j union (make-range i)))
    (~> g graph-idx->id (gvector-add! _ id))
    (~> g graph-id->idx (hash-set! _ id i))
    (~> g graph-base-rels graph/base-rels-id->known-parents  (gvector-add! _ empty/range))
    (~> g graph-base-rels graph/base-rels-id->known-children (gvector-add! _ empty/range))
    (~> g graph-base-rels graph/base-rels-id->known-unrel    (gvector-add! _ empty/range))
    (~> g graph-structure-cache graph/structure-cache-id->ancestors   (gvector-add! _ empty/range))
    (~> g graph-structure-cache graph/structure-cache-id->descendents (gvector-add! _ empty/range))
    (~> g graph-structure-cache graph/structure-cache-id->unrel       (gvector-add! _ empty/range))
    (~> g graph-structure-cache graph/structure-cache-id->unknown     (gvector-add! _ 0:..i)))
(define (graph-parent-child! g p c)
    (define pi (_id->idx g p))
    (define ci (_id->idx g c))
    (define descendent-cache (~> g graph-structure-cache graph/structure-cache-id->descendents))
    (define ancestor-cache   (~> g graph-structure-cache graph/structure-cache-id->ancestors))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define child-known  (~> g graph-base-rels graph/base-rels-id->known-children))
    (define parent-known (~> g graph-base-rels graph/base-rels-id->known-parents))
    (define unrel-known  (~> g graph-base-rels graph/base-rels-id->known-unrel))
    (_update-cache! child-known      pi union    (make-range ci))
    (_update-cache! descendent-cache pi union    (make-range ci))
    (_update-cache! parent-known     pi subtract (make-range ci))
    (_update-cache! ancestor-cache   pi subtract (make-range ci))
    (_update-cache! unrel-cache      pi subtract (make-range ci))
    (_update-cache! unknown-cache    pi subtract (make-range ci))
    (_update-cache! parent-known     ci union    (make-range pi))
    (_update-cache! ancestor-cache   ci union    (make-range pi))
    (_update-cache! child-known      ci subtract (make-range pi))
    (_update-cache! descendent-cache ci subtract (make-range pi))
    (_update-cache! unrel-cache      ci subtract (make-range pi))
    (_update-cache! unknown-cache    ci subtract (make-range pi))
    (_propagate-parent-child g pi ci)
    (_propagate-unrel g))
(define (graph-unrelated! g a b)
    (define ai (_id->idx g a))
    (define bi (_id->idx g b))
    (define unrel-cache      (~> g graph-structure-cache graph/structure-cache-id->unrel))
    (define unknown-cache    (~> g graph-structure-cache graph/structure-cache-id->unknown))
    (define unrel-known  (~> g graph-base-rels graph/base-rels-id->known-unrel))
    (_update-cache! unrel-known   ai union    (make-range bi))
    (_update-cache! unrel-cache   ai union    (make-range bi))
    (_update-cache! unknown-cache ai subtract (make-range bi))
    (_update-cache! unrel-known   bi union    (make-range ai))
    (_update-cache! unrel-cache   bi union    (make-range ai))
    (_update-cache! unknown-cache bi subtract (make-range ai))
    (_propagate-unrel g))


