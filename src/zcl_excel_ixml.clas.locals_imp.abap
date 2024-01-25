*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

INTERFACE lif_ixml_istream.
  INTERFACES zif_excel_ixml_istream.
  DATA sxml_reader TYPE REF TO if_sxml_reader READ-ONLY.
ENDINTERFACE.


INTERFACE lif_ixml_ostream.
  INTERFACES zif_excel_ixml_ostream.
  DATA sxml_writer TYPE REF TO if_sxml_writer READ-ONLY.
  DATA type        TYPE c LENGTH 1            READ-ONLY.
ENDINTERFACE.


CLASS lcl_ixml_unknown DEFINITION
    INHERITING FROM lcl_ixml_root_all
    CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_unknown.

  PROTECTED SECTION.

    DATA type TYPE lcl_ixml=>tv_node_type.

ENDCLASS.


CLASS lcl_ixml_node DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PROTECTED
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node.

    " PRivate SECTION.
  PROTECTED SECTION.

    DATA document         TYPE REF TO lcl_ixml_document.

*    DATA value            TYPE string.
    DATA parent           TYPE REF TO lcl_ixml_node.
    DATA previous_sibling TYPE REF TO lcl_ixml_node.
    DATA next_sibling     TYPE REF TO lcl_ixml_node.
    DATA first_child      TYPE REF TO lcl_ixml_node.
    "! Useful for performance to APPEND
    DATA last_child       TYPE REF TO lcl_ixml_node.

ENDCLASS.


CLASS lcl_ixml_attribute DEFINITION
    INHERITING FROM lcl_ixml_node
    CREATE PROTECTED
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_attribute.

  PRIVATE SECTION.

    DATA name  TYPE string.
    DATA value TYPE string.

ENDCLASS.


CLASS lcl_ixml_character_data DEFINITION
    INHERITING FROM lcl_ixml_node
    CREATE PROTECTED
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_character_data.

ENDCLASS.


CLASS lcl_ixml_document DEFINITION
    INHERITING FROM lcl_ixml_node
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_document.

*    METHODS get_node
*      IMPORTING
*        id          TYPE lcl_ixml_factory=>tv_node_id
*      RETURNING
*        VALUE(rval) TYPE REF TO zif_excel_ixml_node.

  PRIVATE SECTION.

*    DATA nodes                TYPE lcl_ixml_factory=>tt_node.
*    DATA namespaces           TYPE lcl_ixml_factory=>tt_namespace.
*    DATA element_names        TYPE lcl_ixml_factory=>tt_element_name.
*    DATA elements             TYPE lcl_ixml_factory=>tt_element.
*    DATA parse_element_levels TYPE lcl_ixml_factory=>tt_parse_element_level.
*
*    METHODS append_child
*      IMPORTING
*        node        TYPE REF TO lcl_ixml_node
*        new_child   TYPE REF TO zif_excel_ixml_node
*      RETURNING
*        VALUE(rval) TYPE i.
*
*    METHODS get_node_children
*      IMPORTING
*        node_id TYPE lcl_ixml_factory=>tv_node_id
*      RETURNING
*        VALUE(rval) TYPE REF TO zif_excel_ixml_node_list.
*
*    METHODS get_node_iterator
*      IMPORTING
*        node_list TYPE REF TO lcl_ixml_node_list OPTIONAL
*        node_collection TYPE REF TO lcl_ixml_node_collection OPTIONAL
*      RETURNING
*      VALUE(rval) TYPE REF TO lcl_ixml_node_iterator.
*
*    METHODS parse
*      IMPORTING
*        istream     TYPE REF TO lif_ixml_istream
*      RETURNING
*        VALUE(rval) TYPE i.
*
*    METHODS render
*      IMPORTING
*        ostream     TYPE REF TO lif_ixml_ostream
*      RETURNING
*        VALUE(rval) TYPE i.

ENDCLASS.


CLASS lcl_ixml_element DEFINITION
    INHERITING FROM lcl_ixml_node
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_element.

  PRIVATE SECTION.

    DATA name       TYPE string.
    DATA namespace  TYPE string.
    DATA attributes TYPE TABLE OF REF TO lcl_ixml_attribute.

ENDCLASS.


CLASS lcl_ixml_encoding DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_encoding.

  PRIVATE SECTION.

    DATA byte_order    TYPE i.
    DATA character_set TYPE string.

ENDCLASS.


CLASS lcl_ixml_istream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_stream_factory.

  PUBLIC SECTION.

    INTERFACES lif_ixml_istream.

  PRIVATE SECTION.

    CLASS-METHODS create
      IMPORTING
        string      TYPE string
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_istream_string.

ENDCLASS.


CLASS lcl_ixml_istream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_stream_factory.

  PUBLIC SECTION.

    INTERFACES lif_ixml_istream.

  PRIVATE SECTION.

    CLASS-METHODS create
      IMPORTING
        string      TYPE xstring
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_istream_xstring.

ENDCLASS.


CLASS lcl_ixml_named_node_map DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_document.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_named_node_map.

ENDCLASS.


CLASS lcl_ixml_node_collection DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_collection.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_ixml_node_iterator DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_iterator.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_ixml_node_list DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_list.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_ixml_ostream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_ixml_ostream.

  PRIVATE SECTION.

    DATA ref_string TYPE REF TO string.

    CLASS-METHODS create
      IMPORTING
        string      TYPE string
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_ostream_string.

ENDCLASS.


CLASS lcl_ixml_ostream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_ixml_ostream.

  PRIVATE SECTION.

    DATA ref_xstring TYPE REF TO xstring.

    CLASS-METHODS create
      IMPORTING
        xstring      TYPE xstring
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_ostream_xstring.

ENDCLASS.


CLASS lcl_ixml_parser DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_parser.

  PRIVATE SECTION.

    DATA document       TYPE REF TO lcl_ixml_document.
    DATA istream        TYPE REF TO lif_ixml_istream.
    DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.

ENDCLASS.


CLASS lcl_ixml_renderer DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_renderer.

  PRIVATE SECTION.

    DATA document TYPE REF TO lcl_ixml_document.
    DATA ostream  TYPE REF TO lif_ixml_ostream.

    METHODS render_node
      IMPORTING
        io_node TYPE REF TO lcl_ixml_node
      RETURNING
        VALUE(rv_rc) TYPE i.

ENDCLASS.


CLASS lcl_ixml_stream DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream.
ENDCLASS.


CLASS lcl_ixml_stream_factory DEFINITION
    INHERITING FROM lcl_ixml_unknown
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream_factory.
ENDCLASS.


CLASS lcl_ixml_text DEFINITION
    INHERITING FROM lcl_ixml_character_data
    CREATE PRIVATE
    FRIENDS lif_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_text.

  PRIVATE SECTION.

    DATA value TYPE string.

ENDCLASS.


CLASS lcl_ixml IMPLEMENTATION.
  METHOD zif_excel_ixml~create_document.
    DATA(lo_document) = NEW lcl_ixml_document( ).
    lo_document->document = lo_document.
    rval = lo_document.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_encoding.
    DATA encoding TYPE REF TO lcl_ixml_encoding.

    encoding = NEW lcl_ixml_encoding( ).
    encoding->byte_order    = byte_order.
    encoding->character_set = character_set.
    rval = encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_parser.
    DATA parser TYPE REF TO lcl_ixml_parser.

    parser = NEW lcl_ixml_parser( ).
    parser->document       ?= document.
    parser->istream        ?= istream.
    parser->stream_factory  = stream_factory.
    rval = parser.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_renderer.
    DATA renderer TYPE REF TO lcl_ixml_renderer.

    renderer = NEW lcl_ixml_renderer( ).
    renderer->document ?= document.
    renderer->ostream  ?= ostream.
    rval = renderer.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_stream_factory.
    rval = NEW lcl_ixml_stream_factory( ).
  ENDMETHOD.

  METHOD get_singleton.
    IF singleton IS NOT BOUND.
      singleton = NEW lcl_ixml( ).
    ENDIF.
    rval = singleton.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_character_data IMPLEMENTATION.

ENDCLASS.


CLASS lcl_ixml_document IMPLEMENTATION.
*  METHOD append_child.
*    DATA ls_node      TYPE lcl_ixml_factory=>ts_node.
*    DATA lo_new_child TYPE REF TO lcl_ixml_node.
*
*    ls_node-id = lo_new_child->node_id.
*    lo_new_child ?= new_child.
*    ls_node-parent_id = node_id.
*    ls_node-type      = lo_new_child->type.
*    INSERT ls_node INTO TABLE nodes.
*  ENDMETHOD.
*
*  METHOD get_node.
*    READ TABLE nodes REFERENCE INTO DATA(lr_node) WITH TABLE KEY id = id.
*    ASSERT sy-subrc = 0.
*    IF lr_node->object IS BOUND.
*      rval ?= lr_node->object.
*    ELSE.
*      CASE lr_node->type.
*        WHEN zif_excel_ixml_node=>co_node_element.
*          DATA(lo_element) = NEW lcl_ixml_element( ).
*          lo_element->document = me.
*          lo_element->type     = zif_excel_ixml_node=>co_node_element.
*          lo_element->node_id  = id.
*          rval = lo_element.
*        WHEN zif_excel_ixml_node=>co_node_text.
*          DATA(lo_text) = NEW lcl_ixml_text( ).
*          lo_text->document = me.
*          lo_text->type     = zif_excel_ixml_node=>co_node_text.
*          lo_text->node_id  = id.
*          lo_text->value    = lr_node->text.
*          rval = lo_text.
*      ENDCASE.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD get_node_children.
*    DATA(node_list) = NEW lcl_ixml_node_list( ).
*    node_list->document = me.
*    LOOP AT nodes REFERENCE INTO DATA(node)
*         USING KEY by_parent
*         WHERE parent_id = node_id.
*      INSERT node->id INTO TABLE node_list->node_ids.
*    ENDLOOP.
*    rval = node_list.
*  ENDMETHOD.
*
*  METHOD get_node_iterator.
*    DATA(node_iterator) = NEW lcl_ixml_node_iterator( ).
*    node_iterator->document        = me.
*    node_iterator->node_list       = node_list.
*    node_iterator->node_collection = node_collection.
*    rval = node_iterator.
*  ENDMETHOD.
*
*  METHOD parse.
*    DATA lo_reader                  TYPE REF TO if_sxml_reader.
*    DATA lv_current_level           TYPE i.
*    DATA lv_current_element_node_id TYPE lcl_ixml_factory=>tv_node_id.
*    DATA lo_node                    TYPE REF TO if_sxml_node.
*    DATA lo_parse_error             TYPE REF TO cx_sxml_parse_error.
*    DATA lo_node_close              TYPE REF TO if_sxml_close_element.
*    DATA lr_parse_element_level     TYPE REF TO lcl_ixml_factory=>ts_parse_element_level.
*    DATA lo_node_open               TYPE REF TO if_sxml_open_element.
*    DATA ls_node                    TYPE lcl_ixml_factory=>ts_node.
*    DATA lr_namespace               TYPE REF TO lcl_ixml_factory=>ts_namespace.
*    DATA ls_namespace               TYPE lcl_ixml_factory=>ts_namespace.
*    DATA lv_namespace_id            TYPE lcl_ixml_factory=>tv_namespace_id.
*    DATA lr_element_name            TYPE REF TO lcl_ixml_factory=>ts_element_name.
*    DATA ls_element_name            TYPE lcl_ixml_factory=>ts_element_name.
*    DATA ls_element                 TYPE lcl_ixml_factory=>ts_element.
*    DATA ls_attribute               TYPE lcl_ixml_factory=>ts_attribute.
*    DATA ls_parse_element_level     TYPE lcl_ixml_factory=>ts_parse_element_level.
*    DATA lo_node_value              TYPE REF TO if_sxml_value_node.
*
*    lo_reader = istream->sxml_reader.
*    lv_current_level = 0.
*    lv_current_element_node_id = 0.
*
*    DO.
*      TRY.
*          lo_node = lo_reader->read_next_node( ).
*        CATCH cx_sxml_parse_error INTO lo_parse_error.
*          rval = 1. "raise exception type zcx_excel_ixml EXPORTING previous = lo_parse_error.
*          EXIT.
*      ENDTRY.
*      IF lo_node IS NOT BOUND.
*        EXIT.
*      ENDIF.
*
*      CASE lo_node->type.
*        WHEN lo_node->co_nt_attribute.
*          BREAK-POINT."should not happen in OO parsing
*
*        WHEN lo_node->co_nt_element_close.
*          lo_node_close ?= lo_node.
*          DELETE parse_element_levels WHERE level = lv_current_level.
*          lv_current_level = lv_current_level - 1.
*          IF lv_current_level = 0.
*            lv_current_element_node_id = 0.
*          ELSE.
*            READ TABLE parse_element_levels WITH TABLE KEY level = lv_current_level REFERENCE INTO lr_parse_element_level.
*            ASSERT sy-subrc = 0.
*            lv_current_element_node_id = lr_parse_element_level->node_id.
*          ENDIF.
*
*        WHEN lo_node->co_nt_element_open.
*          lo_node_open ?= lo_node.
*
*          ls_node-id        = lines( nodes ) + 1.
*          ls_node-type      = zif_excel_ixml_node=>co_node_element.
*          ls_node-parent_id = lv_current_element_node_id.
*          INSERT ls_node INTO TABLE nodes.
*
*          IF lo_node_open->qname-namespace IS NOT INITIAL.
*            READ TABLE namespaces WITH TABLE KEY by_uri COMPONENTS uri = lo_node_open->qname-namespace REFERENCE INTO lr_namespace.
*            IF sy-subrc <> 0.
*              ls_namespace-id  = lines( namespaces ) + 1.
*              ls_namespace-uri = lo_node_open->qname-namespace.
*              INSERT ls_namespace INTO TABLE namespaces REFERENCE INTO lr_namespace.
*            ENDIF.
*            lv_namespace_id = lr_namespace->id.
*          ELSE.
*            lv_namespace_id = 0.
*          ENDIF.
*
*          READ TABLE element_names WITH TABLE KEY by_name COMPONENTS name         = lo_node_open->qname-name
*                                                                     namespace_id = lv_namespace_id REFERENCE INTO lr_element_name.
*          IF sy-subrc <> 0.
*            ls_element_name-id           = lines( element_names ) + 1.
*            ls_element_name-name         = lo_node_open->qname-name.
*            ls_element_name-namespace_id = lv_namespace_id.
*            INSERT ls_element_name INTO TABLE element_names REFERENCE INTO lr_element_name.
*          ENDIF.
*
*          ls_element-id      = ls_node-id.
*          ls_element-name_id = lr_element_name->id.
*          DATA(lt_sxml_attribute) = lo_node_open->get_attributes( ).
*          LOOP AT lt_sxml_attribute INTO DATA(ls_sxml_attribute).
*            ls_attribute-name  = ls_sxml_attribute->qname-name.
*            ls_attribute-value = ls_sxml_attribute->get_value( ).
*            INSERT ls_attribute INTO TABLE ls_element-attributes.
*          ENDLOOP.
*          INSERT ls_element INTO TABLE elements.
*
*          lv_current_level = lv_current_level + 1.
*          ls_parse_element_level-level   = lv_current_level.
*          ls_parse_element_level-node_id = ls_node-id.
*          INSERT ls_parse_element_level INTO TABLE parse_element_levels.
*
*          lv_current_element_node_id = ls_node-id.
*
*        WHEN lo_node->co_nt_final.
*          BREAK-POINT."should not happen
*
*        WHEN lo_node->co_nt_initial.
*          BREAK-POINT."should not happen
*
*        WHEN lo_node->co_nt_value.
*          lo_node_value ?= lo_node.
*
*          ls_node-id        = lines( nodes ) + 1.
*          ls_node-type      = zif_excel_ixml_node=>co_node_text.
*          ls_node-parent_id = lv_current_element_node_id.
*          ls_node-text      = lo_node_value->get_value( ).
*          INSERT ls_node INTO TABLE nodes.
*
*      ENDCASE.
*    ENDDO.
*  ENDMETHOD.
*
*  METHOD render.
*    DATA lo_writer                  TYPE REF TO if_sxml_writer.
*    DATA lv_current_level           TYPE i.
*    DATA lv_current_element_node_id TYPE lcl_ixml_factory=>tv_node_id.
*    DATA lr_element                 TYPE REF TO lcl_ixml_factory=>ts_element.
*    DATA lr_element_name            TYPE REF TO lcl_ixml_factory=>ts_element_name.
*    DATA lr_namespace               TYPE REF TO lcl_ixml_factory=>ts_namespace.
*    DATA lv_uri                     TYPE string.
*    DATA lo_sxml_element            TYPE REF TO if_sxml_open_element.
*    DATA lt_sxml_attribute          TYPE if_sxml_attribute=>attributes.
*
*    lo_writer = ostream->sxml_writer.
*    lv_current_level = 0.
*    lv_current_element_node_id = 0.
*
*    LOOP AT nodes REFERENCE INTO DATA(node).
*      CASE node->type.
*        WHEN zif_excel_ixml_node=>co_node_element.
*          READ TABLE elements REFERENCE INTO lr_element WITH TABLE KEY id = node->id.
*          READ TABLE element_names REFERENCE INTO lr_element_name WITH TABLE KEY id = node->id.
*          IF lr_element_name->namespace_id <> 0.
*            READ TABLE namespaces REFERENCE INTO lr_namespace WITH TABLE KEY id = lr_element_name->namespace_id.
*            lv_uri = lr_namespace->uri.
*          ENDIF.
*          lo_sxml_element = lo_writer->new_open_element( name = lr_element_name->name ).
*          LOOP AT lr_element->attributes REFERENCE INTO DATA(lr_attribute).
*            lt_sxml_attribute = lo_sxml_element->get_attributes( ).
*            lo_sxml_element->set_attribute( name  = lr_attribute->name
*                                            value = lr_attribute->name ).
*          ENDLOOP.
*      ENDCASE.
*    ENDLOOP.
*  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_element.
    DATA lo_element TYPE REF TO lcl_ixml_element.

    CREATE OBJECT lo_element.
    lo_element->type      = zif_excel_ixml_node=>co_node_element.
    lo_element->name      = name.
    lo_element->namespace = namespace.
    rval = lo_element.
*    DATA lr_namespace    TYPE REF TO lcl_ixml_factory=>ts_namespace.
*    DATA ls_namespace    TYPE lcl_ixml_factory=>ts_namespace.
*    DATA lr_element_name TYPE REF TO lcl_ixml_factory=>ts_element_name.
*    DATA ls_element_name TYPE lcl_ixml_factory=>ts_element_name.
*    DATA ls_element      TYPE lcl_ixml_factory=>ts_element.
*
*    READ TABLE namespaces WITH TABLE KEY by_uri COMPONENTS uri = namespace REFERENCE INTO lr_namespace.
*    IF sy-subrc <> 0.
*      ls_namespace-id  = lines( namespaces ) + 1.
*      ls_namespace-uri = namespace.
*      INSERT ls_namespace INTO TABLE namespaces REFERENCE INTO lr_namespace.
*    ENDIF.
*
*    READ TABLE element_names WITH TABLE KEY by_name COMPONENTS name         = name
*                                                               namespace_id = lr_namespace->id
*         REFERENCE INTO lr_element_name.
*    IF sy-subrc <> 0.
*      ls_element_name-id           = lines( element_names ).
*      ls_element_name-name         = name.
*      ls_element_name-namespace_id = lr_namespace->id.
*      INSERT ls_element_name INTO TABLE element_names REFERENCE INTO lr_element_name.
*    ENDIF.
*
*    ls_element-id      = lines( elements ) + 1.
*    ls_element-name_id = lr_element_name->id.
*
*    CREATE OBJECT ls_element-object.
*    ls_element-object->document = me.
*    ls_element-object->node_id  = 0. " TODO
*    ls_element-object->type     = zif_excel_ixml_node=>co_node_element.
*    INSERT ls_element INTO TABLE elements.
*    rval ?= ls_element-object.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name_ns.
*    DATA lv_namespace_id TYPE lcl_ixml_factory=>tv_namespace_id.
*    DATA lr_namespace    TYPE REF TO lcl_ixml_factory=>ts_namespace.
*    DATA lr_element_name TYPE REF TO lcl_ixml_factory=>ts_element_name.
*    DATA lr_element      TYPE REF TO lcl_ixml_factory=>ts_element.
*    DATA lo_element      TYPE REF TO lcl_ixml_element.
*
*    IF uri IS INITIAL.
*      lv_namespace_id = 0.
*    ELSE.
*      READ TABLE namespaces WITH TABLE KEY by_uri COMPONENTS uri = uri REFERENCE INTO lr_namespace.
*      IF sy-subrc <> 0.
*        RETURN.
*      ENDIF.
*      lv_namespace_id = lr_namespace->id.
*    ENDIF.
*    READ TABLE element_names WITH TABLE KEY by_name COMPONENTS name         = name
*                                                               namespace_id = lv_namespace_id
*         REFERENCE INTO lr_element_name.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    READ TABLE elements WITH TABLE KEY by_name COMPONENTS name_id = lr_element_name->id
*         REFERENCE INTO lr_element.
*    ASSERT sy-subrc = 0.
*    IF lr_element->object IS NOT BOUND.
*      lo_element = NEW lcl_ixml_element( ).
*      lo_element->document = me.
*      lo_element->node_id  = lr_element->id.
*      lr_element->object = lo_element.
*    ENDIF.
*    rval = lr_element->object.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_root_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_standalone.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_element IMPLEMENTATION.
*  METHOD create.
*    rval = NEW lcl_ixml_element( ).
*    rval->document = document.
*    rval->node_id  = node_id.
*  ENDMETHOD.
*
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
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_encoding IMPLEMENTATION.

ENDCLASS.


CLASS lcl_ixml_istream_string IMPLEMENTATION.
  METHOD create.
    DATA xstring TYPE xstring.

    rval = NEW lcl_ixml_istream_string( ).
    xstring = cl_abap_codepage=>convert_to( string ).
    rval->lif_ixml_istream~sxml_reader = cl_sxml_string_reader=>create( input = xstring ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_istream_xstring IMPLEMENTATION.
  METHOD create.
    rval = NEW lcl_ixml_istream_xstring( ).
    rval->lif_ixml_istream~sxml_reader = cl_sxml_string_reader=>create( input = string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_named_node_map IMPLEMENTATION.
  METHOD zif_excel_ixml_named_node_map~create_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_node IMPLEMENTATION.
  METHOD zif_excel_ixml_node~append_child.
    DATA last_child_before TYPE REF TO lcl_ixml_node.

    last_child_before = last_child.

    IF last_child_before IS BOUND.
      last_child_before->next_sibling ?= new_child.
    ELSE.
      first_child ?= new_child.
    ENDIF.

    last_child ?= new_child.
    last_child->parent           = me.
    last_child->previous_sibling = last_child_before.
    last_child->next_sibling     = lcl_ixml=>no_node.
*    rval = document->append_child( node      = me
*                                   new_child = new_child ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~clone.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~create_iterator.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_attributes.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_children.
*    rval = document->get_node_children( node_id ).
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
    DATA lo_cast_text           TYPE REF TO lcl_ixml_text.
    DATA lt_node                TYPE TABLE OF REF TO zif_excel_ixml_node.
    DATA lo_node                TYPE REF TO zif_excel_ixml_node.
    DATA lv_tabix               TYPE i.
    DATA lo_child_node_list     TYPE REF TO zif_excel_ixml_node_list.
    DATA lo_child_node_iterator TYPE REF TO zif_excel_ixml_node_iterator.
    DATA lo_child_node          TYPE REF TO zif_excel_ixml_node.
    DATA lv_node_type           TYPE i.
    DATA lo_text                TYPE REF TO lcl_ixml_text.

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


CLASS lcl_ixml_node_collection IMPLEMENTATION.
  METHOD zif_excel_ixml_node_collection~create_iterator.
*    rval = document->get_node_iterator( node_collection = me ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node_collection~get_length.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_node_iterator IMPLEMENTATION.
  METHOD zif_excel_ixml_node_iterator~get_next.
*    FIELD-SYMBOLS <lt_node_id> TYPE lcl_ixml_factory=>tt_node_id.
*
*    IF node_list IS BOUND.
*      ASSIGN node_list->node_ids TO <lt_node_id>.
*    ELSEIF node_collection IS BOUND.
*      ASSIGN node_collection->node_ids TO <lt_node_id>.
*    ENDIF.
*    IF <lt_node_id> IS ASSIGNED AND index < lines( <lt_node_id> ).
*      index = index + 1.
*      READ TABLE <lt_node_id> INTO DATA(lv_node_id) INDEX index.
*      ASSERT sy-subrc = 0.
*      rval = document->get_node( lv_node_id ).
*    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_node_list IMPLEMENTATION.
  METHOD zif_excel_ixml_node_list~create_iterator.
*    rval = document->get_node_iterator( node_list = me ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_ostream_string IMPLEMENTATION.
  METHOD create.
*    DATA xstring TYPE xstring.
    CREATE OBJECT rval TYPE lcl_ixml_ostream_string.
    GET REFERENCE OF string INTO rval->ref_string.
*    xstring = cl_abap_codepage=>convert_to( string ).
    rval->lif_ixml_ostream~type        = 'C'.
    rval->lif_ixml_ostream~sxml_writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_ostream_xstring IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT rval TYPE lcl_ixml_ostream_xstring.
    GET REFERENCE OF xstring INTO rval->ref_xstring.
    rval->lif_ixml_ostream~type        = 'X'.
    rval->lif_ixml_ostream~sxml_writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_parser IMPLEMENTATION.
  METHOD zif_excel_ixml_parser~add_strip_space_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~parse.
    DATA lo_reader          TYPE REF TO if_sxml_reader.
    DATA lo_node            TYPE REF TO if_sxml_node.
    DATA lo_parse_error     TYPE REF TO cx_sxml_parse_error.
    DATA lo_node_close      TYPE REF TO if_sxml_close_element.
    DATA lo_node_open       TYPE REF TO if_sxml_open_element.
    DATA lo_isxml_element   TYPE REF TO lcl_ixml_element.
    DATA lo_isxml_attribute TYPE REF TO lcl_ixml_attribute.
    DATA lo_node_value      TYPE REF TO if_sxml_value_node.
    DATA lo_isxml_text      TYPE REF TO lcl_ixml_text.

    lo_reader = istream->sxml_reader.

    DO.
      TRY.
          lo_node = lo_reader->read_next_node( ).
        CATCH cx_sxml_parse_error INTO lo_parse_error.
          rval = 1. "raise exception type zcx_excel_ixml EXPORTING previous = lo_parse_error.
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

          DATA(lt_sxml_attribute) = lo_node_open->get_attributes( ).
          LOOP AT lt_sxml_attribute INTO DATA(ls_sxml_attribute).
            CREATE OBJECT lo_isxml_attribute.
            lo_isxml_attribute->name  = ls_sxml_attribute->qname-name.
            lo_isxml_attribute->value = ls_sxml_attribute->get_value( ).
            INSERT lo_isxml_attribute INTO TABLE lo_isxml_element->attributes.
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
*    rval = document->parse( istream ).
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


CLASS lcl_ixml_renderer IMPLEMENTATION.
  METHOD render_node.
    DATA lo_writer                    TYPE REF TO if_sxml_writer.
    DATA lo_element                   TYPE REF TO lcl_ixml_element.
    DATA lo_node                      TYPE REF TO lcl_ixml_node.
    DATA lo_sxml_element              TYPE REF TO if_sxml_open_element.
    DATA lo_attribute                 TYPE REF TO lcl_ixml_attribute.
    DATA lo_text                      TYPE REF TO lcl_ixml_text.
    DATA lo_sxml_value                TYPE REF TO if_sxml_value_node.
    DATA lo_cast_sxml_string_writer   TYPE REF TO cl_sxml_string_writer.
    DATA lv_xstring                   TYPE xstring.
    DATA lo_cast_ixml_ostream_string  TYPE REF TO lcl_ixml_ostream_string.
    DATA lo_cast_ixml_ostream_xstring TYPE REF TO lcl_ixml_ostream_xstring.

    lo_writer = ostream->sxml_writer.

    CASE io_node->type.
      WHEN zif_excel_ixml_node=>co_node_element.
        lo_element ?= lo_node.
        lo_sxml_element = lo_writer->new_open_element( name = lo_element->name ).
        LOOP AT lo_element->attributes INTO lo_attribute.
          lo_sxml_element->set_attribute( name  = lo_attribute->name
                                          value = lo_attribute->value ).
        ENDLOOP.
      WHEN zif_excel_ixml_node=>co_node_text.
        lo_text = CAST lcl_ixml_text( lo_node ).
        lo_sxml_value = lo_writer->new_value( ).
        lo_sxml_value->set_value( lo_text->value ).
    ENDCASE.

    lo_node = io_node->first_child.
    WHILE lo_node IS BOUND.
      render_node( lo_node ).
      lo_node = lo_node->next_sibling.
    ENDWHILE.

    CASE io_node->type.
      WHEN zif_excel_ixml_node=>co_node_element.
        lo_writer->new_close_element( ).
    ENDCASE.

    CASE ostream->type.
      WHEN 'C'.
        lo_cast_sxml_string_writer ?= lo_writer.
        lv_xstring = lo_cast_sxml_string_writer->get_output( ).
        lo_cast_ixml_ostream_string ?= ostream.
        lo_cast_ixml_ostream_string->ref_string->* = cl_abap_codepage=>convert_from( lv_xstring ).
      WHEN 'X'.
        lo_cast_sxml_string_writer ?= lo_writer.
        lo_cast_ixml_ostream_xstring ?= ostream.
        lo_cast_ixml_ostream_xstring->ref_xstring->* = lo_cast_sxml_string_writer->get_output( ).
    ENDCASE.
    rv_rc = 0.
  ENDMETHOD.

  METHOD zif_excel_ixml_renderer~render.
    rval = render_node( document->first_child ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_stream IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_stream_factory IMPLEMENTATION.
  METHOD zif_excel_ixml_stream_factory~create_istream_string.
    rval = lcl_ixml_istream_string=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_istream_xstring.
    rval = lcl_ixml_istream_xstring=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_cstring.
    rval = lcl_ixml_ostream_string=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_xstring.
    rval = lcl_ixml_ostream_xstring=>create( string ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_text IMPLEMENTATION.
ENDCLASS.


CLASS lcl_ixml_unknown IMPLEMENTATION.
  METHOD zif_excel_ixml_unknown~query_interface.
    CASE iid.
      WHEN lcl_ixml=>ixml_iid-element.
        IF type = zif_excel_ixml_node=>co_node_element.
          rval = me.
        ENDIF.
      WHEN lcl_ixml=>ixml_iid-text.
        IF type = zif_excel_ixml_node=>co_node_text.
          rval = me.
        ENDIF.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
