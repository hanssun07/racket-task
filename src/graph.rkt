#lang racket/base

(require
    "utils/ann.rkt"
    "utils.rkt"
    threading
    data/skip-list data/integer-set)

(:typedef IdIdsRef (SkipList Id (IntegerSet Id)))
(:structdef graph : Graph
    ([ids       : (Listof Id)]
     [base-rels : GraphBaseRels]
     [structure : GraphStructureCache]))
(:structdef graph-base-rels : GraphBaseRels
    ([id->known-parents  : IdIdsRef]
     [id->known-children : IdIdsRef]
     [id->known-unrel    : IdIdsRef]))
(:structdef graph-structure-cache : GraphStructureCache
    ([id->cached-ancestors   : IdIdsRef]
     [id->cached-descendents : IdIdsRef]
     [id->cached-unrel       : IdIdsRef]
     [id->cached-unknown     : IdIdsRef]))

(struct graph
    (ids base-rels structure-cache))
(struct graph-base-rels
    (id->known-parents
     id->known-children
     id->known-unrel))
(struct graph-structure-cache
    (id->cached-ancestors
     id->cached-descendents
     id->cached-unrel
     id->cached-unknown))

(: make-graph (-> Graph))

(: graph-relation-of (Graph Id Id -> (U 'self 'child 'parent
                                        'ancestor 'descendent
                                        'unrelated 'unknown)))
(: graph-get-unknown-relation (Graph    -> (List Id Id)))
(: in-graph-node-parents      (Graph Id -> (Sequenceof Id)))
(: in-graph-node-children     (Graph Id -> (Sequenceof Id)))
(: in graph-node-ancestors    (Graph Id -> (Sequenceof Id)))
(: in-graph-node-descendents  (Graph Id -> (Sequenceof Id)))
(: in-graph-heads             (Graph    -> (Sequenceof Id)))
(: in-graph-tails             (Graph    -> (Sequenceof Id)))

(: graph-add-node!      (Graph Id    -> (U Void (^ Exn:Fail))))
(: graph-parent-child!  (Graph Id Id -> (U Void (^ Exn:Fail))))
(: graph-unrelated!     (Graph Id Id -> (U Void (^ Exn:Fail))))


