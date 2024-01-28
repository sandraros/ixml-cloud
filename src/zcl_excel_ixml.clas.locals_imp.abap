*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_bom_utf16_as_character DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    "! UTF-16 BOM corresponding to the system, either big endian or system endian
    CLASS-DATA system_value  TYPE c LENGTH 1.
    CLASS-DATA big_endian    TYPE c LENGTH 1.
    CLASS-DATA little_endian TYPE c LENGTH 1.
ENDCLASS.


INTERFACE lif_isxml_istream.
  INTERFACES zif_excel_ixml_istream.
  DATA sxml_reader TYPE REF TO if_sxml_reader READ-ONLY.
ENDINTERFACE.


INTERFACE lif_isxml_ostream.
  INTERFACES zif_excel_ixml_ostream.
  DATA sxml_writer TYPE REF TO if_sxml_writer READ-ONLY.
  DATA type        TYPE c LENGTH 1            READ-ONLY.
ENDINTERFACE.


CLASS lcl_isxml_unknown DEFINITION
    INHERITING FROM lcl_isxml_root_all
    CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_unknown.

  PROTECTED SECTION.

    DATA type TYPE lcl_isxml=>tv_node_type.

ENDCLASS.


CLASS lcl_isxml_node DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PROTECTED
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node.

    " PRivate SECTION.
  PROTECTED SECTION.

    DATA document         TYPE REF TO lcl_isxml_document.

*    DATA value            TYPE string.
    DATA parent           TYPE REF TO lcl_isxml_node.
    DATA previous_sibling TYPE REF TO lcl_isxml_node.
    DATA next_sibling     TYPE REF TO lcl_isxml_node.
    DATA first_child      TYPE REF TO lcl_isxml_node.
    "! Useful for performance to APPEND
    DATA last_child       TYPE REF TO lcl_isxml_node.

    "! Must be redefined in subclasses
    METHODS render
      IMPORTING
      io_sxml_writer    TYPE REF TO if_sxml_writer
      io_isxml_renderer TYPE REF TO lcl_isxml_renderer
      RETURNING
      VALUE(rv_rc) TYPE i.

ENDCLASS.


CLASS lcl_isxml_attribute DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PROTECTED
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_attribute.

*  PROTECTED SECTION.
*
*    methods render REDEFINITION.

  PRIVATE SECTION.

    DATA prefix TYPE string.
    DATA name   TYPE string.
    DATA value  TYPE string.

ENDCLASS.


CLASS lcl_isxml_character_data DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PROTECTED
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_character_data.

ENDCLASS.


CLASS lcl_isxml_document DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_document.

*    METHODS get_node
*      IMPORTING
*        id          TYPE lcl_isxml_factory=>tv_node_id
*      RETURNING
*        VALUE(rval) TYPE REF TO zif_excel_ixml_node.

  PROTECTED SECTION.

    METHODS render REDEFINITION.

  PRIVATE SECTION.

    DATA encoding TYPE REF TO lcl_isxml_encoding.
    DATA version  TYPE string VALUE '1.0'.

    METHODS get_xml_header_as_string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_xml_header_as_xstring
      RETURNING
        VALUE(rv_result) TYPE xstring.
*    DATA nodes                TYPE lcl_isxml_factory=>tt_node.
*    DATA namespaces           TYPE lcl_isxml_factory=>tt_namespace.
*    DATA element_names        TYPE lcl_isxml_factory=>tt_element_name.
*    DATA elements             TYPE lcl_isxml_factory=>tt_element.
*    DATA parse_element_levels TYPE lcl_isxml_factory=>tt_parse_element_level.
*
*    METHODS append_child
*      IMPORTING
*        node        TYPE REF TO lcl_isxml_node
*        new_child   TYPE REF TO zif_excel_ixml_node
*      RETURNING
*        VALUE(rval) TYPE i.
*
*    METHODS get_node_children
*      IMPORTING
*        node_id TYPE lcl_isxml_factory=>tv_node_id
*      RETURNING
*        VALUE(rval) TYPE REF TO zif_excel_ixml_node_list.
*
*    METHODS get_node_iterator
*      IMPORTING
*        node_list TYPE REF TO lcl_isxml_node_list OPTIONAL
*        node_collection TYPE REF TO lcl_isxml_node_collection OPTIONAL
*      RETURNING
*      VALUE(rval) TYPE REF TO lcl_isxml_node_iterator.
*
*    METHODS parse
*      IMPORTING
*        istream     TYPE REF TO lif_isxml_istream
*      RETURNING
*        VALUE(rval) TYPE i.
*
*    METHODS render
*      IMPORTING
*        ostream     TYPE REF TO lif_isxml_ostream
*      RETURNING
*        VALUE(rval) TYPE i.

ENDCLASS.


CLASS lcl_isxml_element DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_element.

  PROTECTED SECTION.

    METHODS render REDEFINITION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_attribute,
        prefix TYPE string,
        name   TYPE string,
        object TYPE REF TO lcl_isxml_attribute,
      END OF ts_attribute.
    TYPES tt_attribute TYPE SORTED TABLE OF ts_attribute WITH UNIQUE KEY prefix name.

    DATA name       TYPE string.
    DATA prefix     TYPE string.
    DATA namespace  TYPE string.
    DATA attributes TYPE tt_attribute.

ENDCLASS.


CLASS lcl_isxml_encoding DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_encoding.

  PRIVATE SECTION.

    DATA byte_order    TYPE i.
    DATA character_set TYPE string.

ENDCLASS.


CLASS lcl_isxml_istream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_isxml_stream_factory.

  PUBLIC SECTION.

    INTERFACES lif_isxml_istream.

  PRIVATE SECTION.

    CLASS-METHODS create
      IMPORTING
        string      TYPE string
      RETURNING
        VALUE(rval) TYPE REF TO lcl_isxml_istream_string.

ENDCLASS.


CLASS lcl_isxml_istream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_isxml_stream_factory.

  PUBLIC SECTION.

    INTERFACES lif_isxml_istream.

  PRIVATE SECTION.

    CLASS-METHODS create
      IMPORTING
        string      TYPE xstring
      RETURNING
        VALUE(rval) TYPE REF TO lcl_isxml_istream_xstring.

ENDCLASS.


CLASS lcl_isxml_named_node_map DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_isxml_document.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_named_node_map.

ENDCLASS.


CLASS lcl_isxml_node_collection DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_collection.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_isxml_node_iterator DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_iterator.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_isxml_node_list DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_list.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_isxml_ostream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_isxml_ostream.

  PRIVATE SECTION.

    DATA ref_string TYPE REF TO string.

    CLASS-METHODS create
      IMPORTING
        string      TYPE REF TO string
      RETURNING
        VALUE(rval) TYPE REF TO lcl_isxml_ostream_string.

ENDCLASS.


CLASS lcl_isxml_ostream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_isxml_ostream.

  PRIVATE SECTION.

    DATA ref_xstring TYPE REF TO xstring.

    CLASS-METHODS create
      IMPORTING
        xstring      TYPE REF TO xstring
      RETURNING
        VALUE(rval) TYPE REF TO lcl_isxml_ostream_xstring.

ENDCLASS.


CLASS lcl_isxml_parser DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_parser.

  PRIVATE SECTION.

    DATA document       TYPE REF TO lcl_isxml_document.
    DATA istream        TYPE REF TO lif_isxml_istream.
    DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.

ENDCLASS.


CLASS lcl_isxml_renderer DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_renderer.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_namespace,
        level     TYPE i,
        "! Negative number used in index BY_PREFIX, to order eponymous prefixes last level first.
        neg_level TYPE i,
        prefix    TYPE string,
        uri       TYPE string,
      END OF ts_namespace.
    TYPES tt_namespace TYPE SORTED TABLE OF ts_namespace WITH UNIQUE KEY level
                    WITH UNIQUE SORTED KEY by_prefix COMPONENTS prefix neg_level.

    DATA document           TYPE REF TO lcl_isxml_document.
    DATA ostream            TYPE REF TO lif_isxml_ostream.
    DATA current_level      TYPE i.
    DATA current_namespaces TYPE tt_namespace.

*    METHODS render_node
*      IMPORTING
*        io_node TYPE REF TO lcl_isxml_node
*      RETURNING
*        VALUE(rv_rc) TYPE i.

ENDCLASS.


CLASS lcl_isxml_stream DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream.
ENDCLASS.


CLASS lcl_isxml_stream_factory DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream_factory.
ENDCLASS.


CLASS lcl_isxml_text DEFINITION
    INHERITING FROM lcl_isxml_character_data
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_text.

  PRIVATE SECTION.

    DATA value TYPE string.

ENDCLASS.


CLASS lcl_bom_utf16_as_character IMPLEMENTATION.
  METHOD class_constructor.
    TYPES ty_one_character TYPE c LENGTH 1.

    DATA lv_string      TYPE string.
    DATA lv_bom_4_bytes TYPE x LENGTH 4.

    FIELD-SYMBOLS <lv_bom_character> TYPE ty_one_character.

    CALL TRANSFORMATION id
         SOURCE root = space
         RESULT XML lv_string.
    system_value = substring( val = lv_string
                              off = 0
                              len = 1 ).

    lv_bom_4_bytes = cl_abap_char_utilities=>byte_order_mark_big.
    ASSIGN lv_bom_4_bytes TO <lv_bom_character> CASTING.
    big_endian = <lv_bom_character>.

    lv_bom_4_bytes = cl_abap_char_utilities=>byte_order_mark_little.
    ASSIGN lv_bom_4_bytes TO <lv_bom_character> CASTING.
    little_endian = <lv_bom_character>.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml IMPLEMENTATION.
  METHOD zif_excel_ixml~create_document.
    DATA lo_document TYPE REF TO lcl_isxml_document.

    CREATE OBJECT lo_document.
    lo_document->document = lo_document.
    rval = lo_document.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_encoding.
    DATA lo_encoding TYPE REF TO lcl_isxml_encoding.

    CREATE OBJECT lo_encoding.
    lo_encoding->byte_order    = byte_order.
    lo_encoding->character_set = character_set.
    rval = lo_encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_parser.
    DATA lo_parser TYPE REF TO lcl_isxml_parser.

    CREATE OBJECT lo_parser.
    lo_parser->document       ?= document.
    lo_parser->istream        ?= istream.
    lo_parser->stream_factory  = stream_factory.
    rval = lo_parser.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_renderer.
    DATA lo_renderer TYPE REF TO lcl_isxml_renderer.

    CREATE OBJECT lo_renderer.
    lo_renderer->document ?= document.
    lo_renderer->ostream  ?= ostream.
    rval = lo_renderer.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_stream_factory.
    DATA lo_isxml_stream_factory TYPE REF TO lcl_isxml_stream_factory.

    CREATE OBJECT lo_isxml_stream_factory.
    rval = lo_isxml_stream_factory.
  ENDMETHOD.

  METHOD get_singleton.
    IF singleton IS NOT BOUND.
      CREATE OBJECT singleton.
    ENDIF.
    rval = singleton.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_character_data IMPLEMENTATION.

ENDCLASS.


CLASS lcl_isxml_document IMPLEMENTATION.
  METHOD get_xml_header_as_string.
    DATA lt_string TYPE TABLE OF string.
    DATA lv_string TYPE string.

    IF version IS NOT INITIAL.
      INSERT |version="{ version }"| INTO TABLE lt_string.
    ENDIF.
    INSERT `encoding="utf-16"` INTO TABLE lt_string.
    lv_string = |{ lcl_bom_utf16_as_character=>system_value }<?xml { concat_lines_of( table = lt_string
                                                                                      sep   = ` ` ) }?>|.
    rv_result = lv_string.
  ENDMETHOD.

  METHOD get_xml_header_as_xstring.
    DATA lt_string TYPE TABLE OF string.
    DATA lv_string TYPE string.

    IF version IS NOT INITIAL.
      INSERT |version="{ version }"| INTO TABLE lt_string.
    ENDIF.
    IF encoding IS BOUND.
      INSERT |encoding="{ to_lower( encoding->character_set ) }"| INTO TABLE lt_string.
    ENDIF.
    IF lt_string IS NOT INITIAL.
      lv_string = |<?xml { concat_lines_of( table = lt_string
                                            sep   = ` ` ) }?>|.
    ENDIF.
    rv_result = cl_abap_codepage=>convert_to( lv_string ).
  ENDMETHOD.

  METHOD render.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_element.
    DATA lo_element TYPE REF TO lcl_isxml_element.

    CREATE OBJECT lo_element.
    lo_element->type      = zif_excel_ixml_node=>co_node_element.
    lo_element->name      = name.
    lo_element->namespace = namespace.
    rval = lo_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element.
    DATA lo_element TYPE REF TO lcl_isxml_element.

    CREATE OBJECT lo_element.
    lo_element->type = zif_excel_ixml_node=>co_node_element.
    lo_element->name = name.

    parent->append_child( lo_element ).

    rval = lo_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element_ns.
    DATA lo_element TYPE REF TO lcl_isxml_element.

    CREATE OBJECT lo_element.
    lo_element->type   = zif_excel_ixml_node=>co_node_element.
    lo_element->name   = name.
    lo_element->prefix = prefix.

    parent->append_child( lo_element ).

    rval = lo_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_root_element.
    rval ?= first_child.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_encoding.
    me->encoding ?= encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_standalone.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_element IMPLEMENTATION.
  METHOD render.
    DATA ls_namespace         TYPE lcl_isxml_renderer=>ts_namespace.
    DATA lr_namespace         TYPE REF TO lcl_isxml_renderer=>ts_namespace.
    DATA lv_nsuri             TYPE string.
    DATA lo_sxml_open_element TYPE REF TO if_sxml_open_element.
    DATA lr_isxml_attribute   TYPE REF TO lcl_isxml_element=>ts_attribute.
    DATA lo_isxml_child_node  TYPE REF TO lcl_isxml_node.

    FIELD-SYMBOLS <ls_attribute> TYPE lcl_isxml_element=>ts_attribute.

    io_isxml_renderer->current_level = io_isxml_renderer->current_level + 1.
    LOOP AT attributes ASSIGNING <ls_attribute>
         WHERE prefix = 'xmlns'.
      ls_namespace-level     = io_isxml_renderer->current_level.
      ls_namespace-neg_level = -1 * io_isxml_renderer->current_level.
      ls_namespace-prefix    = <ls_attribute>-object->name.
      ls_namespace-uri       = <ls_attribute>-object->value.
      INSERT ls_namespace INTO TABLE io_isxml_renderer->current_namespaces.
    ENDLOOP.

    " IXML does accept a namespace prefix which is not registered yet for namespaces (can be later defined via set_attribute( name = 'xmlns:xx' )),
    " while SXML doesn't accept a namespace prefix without URI.
    IF prefix IS NOT INITIAL.
      READ TABLE io_isxml_renderer->current_namespaces
           WITH KEY by_prefix COMPONENTS prefix = prefix
           REFERENCE INTO lr_namespace.
      IF sy-subrc <> 0.
        " prefix defined which has
        RAISE EXCEPTION TYPE zcx_excel_ixml.
      ENDIF.
      lv_nsuri = lr_namespace->uri.
    ENDIF.
    lo_sxml_open_element = io_sxml_writer->new_open_element( name   = name
                                                             nsuri  = lv_nsuri
                                                             prefix = prefix ).
    io_sxml_writer->write_node( lo_sxml_open_element ).

    LOOP AT attributes REFERENCE INTO lr_isxml_attribute
         WHERE prefix <> 'xmlns'.
      IF lr_isxml_attribute->prefix IS NOT INITIAL.
        READ TABLE io_isxml_renderer->current_namespaces
             WITH KEY by_prefix COMPONENTS prefix = lr_isxml_attribute->prefix
             REFERENCE INTO lr_namespace.
        IF sy-subrc <> 0.
          " prefix defined which has
          RAISE EXCEPTION TYPE zcx_excel_ixml.
        ENDIF.
        lv_nsuri = lr_namespace->uri.
      ENDIF.
      lo_sxml_open_element->set_attribute( name   = lr_isxml_attribute->name
                                           nsuri  = lv_nsuri
                                           prefix = lr_isxml_attribute->prefix
                                           value  = lr_isxml_attribute->object->value ).
    ENDLOOP.

    lo_isxml_child_node = first_child.
    WHILE lo_isxml_child_node IS BOUND.
      lo_isxml_child_node->render( io_sxml_writer    = io_sxml_writer
                                   io_isxml_renderer = io_isxml_renderer ).
      lo_isxml_child_node = lo_isxml_child_node->next_sibling.
    ENDWHILE.

    io_sxml_writer->close_element( ).

    DELETE io_isxml_renderer->current_namespaces WHERE level = io_isxml_renderer->current_level.
    io_isxml_renderer->current_level = io_isxml_renderer->current_level - 1.

    rv_rc = 0.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~find_from_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~find_from_name_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute_node_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_elements_by_tag_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_elements_by_tag_name_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~remove_attribute_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~set_attribute.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~set_attribute_ns.
    DATA lv_colon_position  TYPE i.
    DATA lv_prefix          TYPE string.
    DATA lv_name            TYPE string.
    DATA ls_isxml_attribute TYPE lcl_isxml_element=>ts_attribute.

    lv_colon_position = find( val = name
                              sub = ':' ).
    IF lv_colon_position >= 0.
      lv_prefix = substring( val = name
                             off = 0
                             len = lv_colon_position ).
      lv_name = substring( val = name
                           off = lv_colon_position + 1 ).
    ELSE.
      lv_prefix = ''.
      lv_name = name.
    ENDIF.

    ls_isxml_attribute-prefix = lv_prefix.
    ls_isxml_attribute-name   = lv_name.
    CREATE OBJECT ls_isxml_attribute-object.
    ls_isxml_attribute-object->type   = zif_excel_ixml_node=>co_node_attribute.
    ls_isxml_attribute-object->prefix = lv_prefix.
    ls_isxml_attribute-object->name   = lv_name.
    ls_isxml_attribute-object->value  = value.
    INSERT ls_isxml_attribute INTO TABLE attributes.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_encoding IMPLEMENTATION.

ENDCLASS.


CLASS lcl_isxml_istream_string IMPLEMENTATION.
  METHOD create.
    DATA xstring TYPE xstring.

    CREATE OBJECT rval.
    xstring = cl_abap_codepage=>convert_to( string ).
    rval->lif_isxml_istream~sxml_reader = cl_sxml_string_reader=>create( input = xstring ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_istream_xstring IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT rval.
    rval->lif_isxml_istream~sxml_reader = cl_sxml_string_reader=>create( input = string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_named_node_map IMPLEMENTATION.
  METHOD zif_excel_ixml_named_node_map~create_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node IMPLEMENTATION.
  METHOD render.
    " Must be redefined in subclasses.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~append_child.
    DATA last_child_before TYPE REF TO lcl_isxml_node.

    last_child_before = last_child.

    IF last_child_before IS BOUND.
      last_child_before->next_sibling ?= new_child.
    ELSE.
      first_child ?= new_child.
    ENDIF.

    last_child ?= new_child.
    last_child->parent           = me.
    last_child->previous_sibling = last_child_before.
    last_child->next_sibling     = lcl_isxml=>no_node.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~clone.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~create_iterator.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_attributes.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_children.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_first_child.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_namespace_prefix.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_namespace_uri.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_next.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_type.
    rval = type.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_value.
    DATA lo_cast_text           TYPE REF TO lcl_isxml_text.
    DATA lt_node                TYPE TABLE OF REF TO zif_excel_ixml_node.
    DATA lo_node                TYPE REF TO zif_excel_ixml_node.
    DATA lv_tabix               TYPE i.
    DATA lo_child_node_list     TYPE REF TO zif_excel_ixml_node_list.
    DATA lo_child_node_iterator TYPE REF TO zif_excel_ixml_node_iterator.
    DATA lo_child_node          TYPE REF TO zif_excel_ixml_node.
    DATA lv_node_type           TYPE i.
    DATA lo_text                TYPE REF TO lcl_isxml_text.

    IF type = zif_excel_ixml_node=>co_node_text.
      lo_cast_text ?= me.
      rval = lo_cast_text->value.
      RETURN.
    ENDIF.

    INSERT me INTO TABLE lt_node.

    LOOP AT lt_node INTO lo_node.
      lv_tabix = sy-tabix.
      lo_child_node_list = lo_node->get_children( ).
      lo_child_node_iterator = lo_child_node_list->create_iterator( ).
      lo_child_node = lo_child_node_iterator->get_next( ).
      WHILE lo_child_node IS BOUND.
        lv_tabix = lv_tabix + 1.
        INSERT lo_child_node INTO lt_node INDEX lv_tabix.
        lo_child_node = lo_child_node_iterator->get_next( ).
      ENDWHILE.
    ENDLOOP.

    LOOP AT lt_node INTO lo_node.
      lv_node_type = lo_node->get_type( ).
      CASE lv_node_type.
        WHEN zif_excel_ixml_node=>co_node_text.
          lo_text ?= lo_node.
          rval = rval && lo_text->value."get_value( ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_namespace_prefix.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_value.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node_collection IMPLEMENTATION.
  METHOD zif_excel_ixml_node_collection~create_iterator.
  ENDMETHOD.

  METHOD zif_excel_ixml_node_collection~get_length.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node_iterator IMPLEMENTATION.
  METHOD zif_excel_ixml_node_iterator~get_next.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node_list IMPLEMENTATION.
  METHOD zif_excel_ixml_node_list~create_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_ostream_string IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT rval TYPE lcl_isxml_ostream_string.
    rval->ref_string                    = string.
    rval->lif_isxml_ostream~type        = 'C'.
    rval->lif_isxml_ostream~sxml_writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_ostream_xstring IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT rval TYPE lcl_isxml_ostream_xstring.
    rval->ref_xstring                   = xstring.
    rval->lif_isxml_ostream~type        = 'X'.
    rval->lif_isxml_ostream~sxml_writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_parser IMPLEMENTATION.
  METHOD zif_excel_ixml_parser~add_strip_space_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~parse.
    DATA lo_reader          TYPE REF TO if_sxml_reader.
    DATA lo_node            TYPE REF TO if_sxml_node.
    DATA lo_parse_error     TYPE REF TO cx_sxml_parse_error.
    DATA lo_node_close      TYPE REF TO if_sxml_close_element.
    DATA lo_node_open       TYPE REF TO if_sxml_open_element.
    DATA lo_isxml_element   TYPE REF TO lcl_isxml_element.
    DATA lt_sxml_attribute  TYPE if_sxml_attribute=>attributes.
    DATA lo_sxml_attribute  TYPE REF TO if_sxml_attribute.
    DATA ls_isxml_attribute TYPE lcl_isxml_element=>ts_attribute.
    DATA lo_node_value      TYPE REF TO if_sxml_value_node.
    DATA lo_isxml_text      TYPE REF TO lcl_isxml_text.
    DATA lo_isxml_attribute TYPE REF TO lcl_isxml_attribute.

    lo_reader = istream->sxml_reader.

    DO.
      TRY.
          lo_node = lo_reader->read_next_node( ).
        CATCH cx_sxml_parse_error INTO lo_parse_error.
          rval = lcl_isxml=>ixml_mr-parser_error.
          EXIT.
      ENDTRY.
      IF lo_node IS NOT BOUND.
        EXIT.
      ENDIF.

      CASE lo_node->type.
        WHEN lo_node->co_nt_attribute.
          BREAK-POINT."should not happen in OO parsing?

        WHEN lo_node->co_nt_element_close.
          lo_node_close ?= lo_node.

        WHEN lo_node->co_nt_element_open.
          lo_node_open ?= lo_node.
          CREATE OBJECT lo_isxml_element.
          lo_isxml_element->type      = zif_excel_ixml_node=>co_node_element.
          lo_isxml_element->name      = lo_node_open->qname-name.
          lo_isxml_element->namespace = lo_node_open->qname-namespace.

          lt_sxml_attribute = lo_node_open->get_attributes( ).
          LOOP AT lt_sxml_attribute INTO lo_sxml_attribute.
            ls_isxml_attribute-name   = lo_sxml_attribute->qname-name.
            ls_isxml_attribute-prefix = lo_sxml_attribute->prefix.
            CREATE OBJECT ls_isxml_attribute-object.
            ls_isxml_attribute-object->prefix = lo_sxml_attribute->prefix.
            ls_isxml_attribute-object->name   = lo_sxml_attribute->qname-name.
            ls_isxml_attribute-object->value  = lo_sxml_attribute->get_value( ).
            INSERT ls_isxml_attribute INTO TABLE lo_isxml_element->attributes.
          ENDLOOP.

        WHEN lo_node->co_nt_final.
          BREAK-POINT."should not happen?

        WHEN lo_node->co_nt_initial.
          BREAK-POINT."should not happen?

        WHEN lo_node->co_nt_value.
          lo_node_value ?= lo_node.
          CREATE OBJECT lo_isxml_text.
          lo_isxml_text->type  = zif_excel_ixml_node=>co_node_attribute.
          lo_isxml_text->value = lo_node_value->get_value( ).

      ENDCASE.
    ENDDO.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_normalizing.
    IF is_normalizing = abap_true.
      istream->sxml_reader->set_option( if_sxml_reader=>co_opt_normalizing ).
    ELSE.
      istream->sxml_reader->set_option( if_sxml_reader=>co_opt_keep_whitespace ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_validating.
    IF mode <> zif_excel_ixml_parser=>co_no_validation.
      RAISE EXCEPTION TYPE zcx_excel_ixml_not_implemented.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_renderer IMPLEMENTATION.
  METHOD zif_excel_ixml_renderer~render.
    DATA lo_sxml_string_writer    TYPE REF TO cl_sxml_string_writer.
    DATA lv_xstring               TYPE xstring.
    DATA lo_isxml_ostream_string  TYPE REF TO lcl_isxml_ostream_string.
    DATA xml_header_as_xstring    TYPE xstring.
    DATA lo_isxml_ostream_xstring TYPE REF TO lcl_isxml_ostream_xstring.
    DATA xml_body_as_xstring      TYPE xstring.

    rval = document->first_child->render( io_sxml_writer    = ostream->sxml_writer
                                          io_isxml_renderer = me ).

    CASE ostream->type.
      WHEN 'C'.
        lo_sxml_string_writer ?= ostream->sxml_writer.
        lv_xstring = lo_sxml_string_writer->get_output( ).
        lo_isxml_ostream_string ?= ostream.
        lo_isxml_ostream_string->ref_string->* = document->get_xml_header_as_string( ) && cl_abap_codepage=>convert_from(
                                                                                              lv_xstring ).
      WHEN 'X'.
        xml_header_as_xstring = document->get_xml_header_as_xstring( ).
        lo_sxml_string_writer ?= ostream->sxml_writer.
        lo_isxml_ostream_xstring ?= ostream.
        xml_body_as_xstring = lo_sxml_string_writer->get_output( ).
        CONCATENATE xml_header_as_xstring
                    xml_body_as_xstring
                    INTO lo_isxml_ostream_xstring->ref_xstring->*
                    IN BYTE MODE.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_stream IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_stream_factory IMPLEMENTATION.
  METHOD zif_excel_ixml_stream_factory~create_istream_string.
    rval = lcl_isxml_istream_string=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_istream_xstring.
    rval = lcl_isxml_istream_xstring=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_cstring.
    rval = lcl_isxml_ostream_string=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_xstring.
    rval = lcl_isxml_ostream_xstring=>create( string ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_text IMPLEMENTATION.
ENDCLASS.


CLASS lcl_isxml_unknown IMPLEMENTATION.
  METHOD zif_excel_ixml_unknown~query_interface.
    CASE iid.
      WHEN lcl_isxml=>ixml_iid-element.
        IF type = zif_excel_ixml_node=>co_node_element.
          rval = me.
        ENDIF.
      WHEN lcl_isxml=>ixml_iid-text.
        IF type = zif_excel_ixml_node=>co_node_text.
          rval = me.
        ENDIF.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
