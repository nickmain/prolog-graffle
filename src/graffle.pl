% (c) 2016, David N. Main - see LICENSE file

:- module( graffle, [plist_to_graffle/2] ).

:- use_module(util).
:- use_module(rtf).

% Interpret a plist as an omnigraffle diagram and add facts to a dynamic module
plist_to_graffle(Plist, GrafModule) :-
    gensym(graffle, GrafModule),
    Context = context{dict: Plist, module: GrafModule, canvas_id: none},
    assert_doc_metadata(Context, _),
    forall(assert_canvases(Context, _), true).

% Graffle document metadata
assert_doc_metadata -->
    sub_dict('UserInfo'), 
    key_facts( kMDItemDescription, doc_description ),
    key_facts( kMDItemComments, doc_comments ),
    key_facts( kMDItemAuthors, doc_author ),
    key_facts( kMDItemCopyright, doc_copyright ),
    key_facts( kMDItemProjects, doc_project ),
    key_facts( kMDItemLanguages, doc_language ),
    key_facts( kMDItemKeywords, doc_keyword ),
    key_facts( kMDItemOrganizations, doc_organization ),
    key_facts( kMDItemSubject, doc_subject ),
    key_facts( kMDItemVersion, doc_version ).

% get each canvas - explicit array or plist itself in case of only one canvas
each_canvas -->
    ( dict_get('Sheets',Canvases) ; 
      get(dict, Doc), {Canvases = [Doc]}),
    !,
    set(canvases, Canvases),
    each_sub_dict(Canvases).

assert_canvases -->
    each_canvas,
    dict_get('UniqueID', CanvasId),
    dict_get('SheetTitle', Title),
    dict_get('GraphicsList', Graphics),

    set(canvas_id, CanvasId),
    assert_fact(canvas(CanvasId, Title)),

    each_sub_dict(Graphics),
    assert_graphics.

assert_graphics -->
    dict_get('ID', Id),
    dict_get('Class', Class),
    get(canvas_id, CanvasId),
    { term_to_atom(CanvasId-Id, GraphicId) },
    assert_fact(graphic(GraphicId, CanvasId, Class)),
    assert_note(GraphicId),
    assert_name(GraphicId),
    assert_canvas_link(GraphicId),
    assert_graphic_link(GraphicId),
    assert_url_link(GraphicId),
    (
        assert_shape(GraphicId, Class) ;
        assert_line(GraphicId, Class) ;
        assert_group(GraphicId, Class) ;
        assert_subgraph(GraphicId, Class) ;
        assert_table(GraphicId, Class) ;
        {true}
    ).

assert_canvas_link(GraphicId) -->
    dict_get(['Link','documentJump','Type'], 6),
    dict_get(['Link','documentJump','Worksheet'], CanvasIndex),
    get(canvases, Canvases),
    { nth0(CanvasIndex, Canvases, TargetCanvas),
      TargetId = TargetCanvas.get('UniqueID') },
    assert_fact(canvas_link(GraphicId, TargetId)),
    ! ; {true}.

assert_url_link(GraphicId) -->
    dict_get(['Link','url'], URL),
    assert_fact(url_link(GraphicId, URL)),
    ! ; {true}.

assert_graphic_link(GraphicId) -->
    dict_get(['Link','documentJump','Type'], 1),
    dict_get(['Link','documentJump','Worksheet'], CanvasIndex),
    dict_get(['Link','documentJump','Graphic'], GraphicIndex),    
    get(canvases, Canvases),
    { nth0(CanvasIndex, Canvases, TargetCanvas),
      CanvasId = TargetCanvas.get('UniqueID'),
      Graphics = TargetCanvas.get('GraphicsList'),
      nth0(GraphicIndex, Graphics, TargetGraphic),
      Id = TargetGraphic.get('ID'),
      term_to_atom(CanvasId-Id, TargetId) },
    assert_fact(graphic_link(GraphicId, TargetId)),
    ! ; {true}.

assert_note(GraphicId) -->
    dict_get('Notes', NoteRTF), 
    { rtf_to_plain( NoteRTF, Note ) },
    assert_fact(graphic_note(GraphicId, Note)),
    ! ; {true}. 

assert_name(GraphicId) -->
    dict_get('Name', Name), 
    assert_fact(graphic_name(GraphicId, Name)),
    ! ; {true}. 

assert_table(GraphicId, 'TableGroup') -->
    assert_fact(table_group(GraphicId)),
    assert_group(GraphicId, 'Group').

assert_subgraph(GraphicId, 'Group') -->
    dict_get(isSubgraph, _),
    dict_get('Graphics', Graphics),   
    { 
        append(_, [Last], Graphics), % last shape is the background
        Id = Last.get('ID')        
    }, 
    get(canvas_id, CanvasId),
    { term_to_atom(CanvasId-Id, ShapeId) },    
    assert_fact(subgraph(GraphicId, ShapeId)).

assert_group(GraphicId, 'Group') -->
    assert_fact(group(GraphicId)),
    dict_get('Graphics', Graphics),   
     
    each_sub_dict(Graphics),
    dict_get('ID', Id),
    get(canvas_id, CanvasId),
    { term_to_atom(CanvasId-Id, MemberId) },    
    assert_fact(group_member(GraphicId, MemberId)),    
    assert_graphics.

assert_shape(GraphicId, 'ShapedGraphic') -->
    dict_get('Shape', Shape, 'Rectangle'),
    dashed( Dashed ),
    assert_fact(shape(GraphicId, Shape, Dashed)),
    assert_bounds(GraphicId),
    assert_text(GraphicId),
    assert_line_label(GraphicId),
    assert_shape_connections(GraphicId).

assert_line(GraphicId, 'LineGraphic') -->
    dict_get(['Style','stroke','HeadArrow'], HeadArrow, none),
    dict_get(['Style','stroke','TailArrow'], TailArrow, none),
    dashed( Dashed ),
    assert_fact(line(GraphicId, HeadArrow, TailArrow, Dashed)),
    assert_connection(GraphicId, 'Head', head),
    assert_connection(GraphicId, 'Tail', tail).

assert_shape_connections(GraphicId) -->
    dict_get('isConnectedShape', _), 
    assert_connection(GraphicId, 'Head', head),
    assert_connection(GraphicId, 'Tail', tail),
    ! ;
    {true}.

assert_bounds(GraphicId) -->
    dict_get('Bounds', Bounds), 
    { read_term_from_atom(Bounds, {{X,Y},{W,H}}, []) },
    assert_fact(shape_bounds(GraphicId, bounds(X,Y,W,H))),
    ! ; {true}. 

assert_text(GraphicId) -->
    dict_get(['Text', 'Text'], RTF), 
    { rtf_to_plain( RTF, Text ) },
    assert_fact(shape_text(GraphicId, Text)),
    ! ; {true}. 

assert_line_label(GraphicId) -->
    dict_get(['Line', 'ID'], Target), 
    dict_get(['Line', 'Position'], Posn), 
    get(canvas_id, CanvasId),
    { term_to_atom(CanvasId-Target, LineId) },  
    assert_fact(line_label(GraphicId, LineId, Posn)),
    ! ; {true}. 

assert_connection(GraphicId, Key, Type) -->
    dict_get([Key, 'ID'], Target), 
    get(canvas_id, CanvasId),
    { term_to_atom(CanvasId-Target, TargetId) },
    assert_fact(connection(GraphicId, TargetId, Type)),
    ! ; {true}. 

% whether graphic is dashed or solid
dashed( Dashed ) -->
    dict_get( ['Style','stroke','Pattern'], _ ),
    { Dashed = dashed },
    ! ;
    { Dashed = solid }.


% assert a fact in the context module
assert_fact(Fact, Ctxt, Ctxt) :-
    Mod = Ctxt.get(module),
    assert(Mod:Fact).

% assert facts in the context module
% if the value is a list then assert a fact for each member
assert_facts(Pred, Value, Ctxt, Ctxt) :-
    Mod = Ctxt.get(module),
    (
        is_list(Value) ->
            forall(member(M, Value), (Fact =.. [Pred, M], assert(Mod:Fact)))
        ; 
            Fact =.. [Pred, Value],
            assert(Mod:Fact)
    ).

% assert facts in the context module for each value of the given key
key_facts(Key, Pred) -->
    dict_get(Key, Value),
    assert_facts(Pred, Value),
    ! ; {true}.    
