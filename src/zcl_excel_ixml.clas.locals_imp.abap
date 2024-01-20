*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_ixml_document DEFINITION DEFERRED.
CLASS lcl_ixml_element DEFINITION DEFERRED.
CLASS lcl_ixml_encoding DEFINITION DEFERRED.
CLASS lcl_ixml_istream_string DEFINITION DEFERRED.
CLASS lcl_ixml_istream_xstring DEFINITION DEFERRED.
CLASS lcl_ixml_ostream_string DEFINITION DEFERRED.
CLASS lcl_ixml_ostream_xstring DEFINITION DEFERRED.
CLASS lcl_ixml_parser DEFINITION DEFERRED.
CLASS lcl_ixml_renderer DEFINITION DEFERRED.
CLASS lcl_ixml_stream_factory DEFINITION DEFERRED.


INTERFACE lif_ixml_istream.
  INTERFACES zif_excel_ixml_istream.
  DATA sxml_reader TYPE REF TO if_sxml_reader READ-ONLY.
ENDINTERFACE.


CLASS lcl_ixml_document DEFINITION
    FRIENDS lcl_ixml_element.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_document.

    TYPES tv_node_id   TYPE i.

    METHODS parse
      IMPORTING
        istream     TYPE REF TO lif_ixml_istream
      RETURNING
        VALUE(rval) TYPE i.

  PRIVATE SECTION.

    TYPES tv_node_type TYPE i.
    TYPES:
      BEGIN OF ts_node,
        id        TYPE tv_node_id,
        type      TYPE tv_node_type,
        parent_id TYPE tv_node_id,
      END OF ts_node.
    TYPES tt_node         TYPE HASHED TABLE OF ts_node WITH UNIQUE KEY id.
    TYPES tv_namespace_id TYPE i.
    TYPES:
      BEGIN OF ts_namespace,
        id  TYPE tv_namespace_id,
        uri TYPE string,
      END OF ts_namespace.
    TYPES tt_namespace TYPE HASHED TABLE OF ts_namespace WITH UNIQUE KEY id
                    WITH UNIQUE HASHED KEY by_uri COMPONENTS uri.
    TYPES:
      BEGIN OF ts_element,
        id           TYPE tv_node_id,
        name         TYPE string,
        namespace_id TYPE tv_namespace_id,
        attributes   TYPE STANDARD TABLE OF REF TO if_sxml_attribute WITH EMPTY KEY,
        object       TYPE REF TO lcl_ixml_element,
      END OF ts_element.
    TYPES tt_element TYPE HASHED TABLE OF ts_element WITH UNIQUE KEY id
                    WITH NON-UNIQUE SORTED KEY by_name COMPONENTS name namespace_id.
    TYPES:
      BEGIN OF ts_parse_element_level,
        level   TYPE i,
        node_id TYPE tv_node_id,
      END OF ts_parse_element_level.
    TYPES tt_parse_element_level TYPE HASHED TABLE OF ts_parse_element_level WITH UNIQUE KEY level.

    CONSTANTS:
      BEGIN OF c_node_type,
        element TYPE tv_node_type VALUE 1,
      END OF c_node_type.

    DATA istream              TYPE REF TO lif_ixml_istream.
    DATA nodes                TYPE tt_node.
    DATA namespaces           TYPE tt_namespace.
    DATA elements             TYPE tt_element.
    DATA parse_element_levels TYPE tt_parse_element_level.

ENDCLASS.


CLASS lcl_ixml_element DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_element.
    CLASS-METHODS create
      IMPORTING
        document    TYPE REF TO lcl_ixml_document
        node_id     TYPE i
      RETURNING
        value(rval) TYPE REF TO lcl_ixml_element.

  PRIVATE SECTION.

    DATA document TYPE REF TO lcl_ixml_document.
    DATA node_id TYPE lcl_ixml_document=>tv_node_id.

ENDCLASS.


CLASS lcl_ixml_encoding DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_excel_ixml_encoding.
ENDCLASS.


CLASS lcl_ixml_istream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_stream_factory.

  PUBLIC SECTION.

    INTERFACES lif_ixml_istream.

    CLASS-METHODS create
      IMPORTING
        string      TYPE string
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_istream_string.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_ixml_istream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_stream_factory.

  PUBLIC SECTION.

    INTERFACES lif_ixml_istream.

    CLASS-METHODS create
      IMPORTING
        string      TYPE xstring
      RETURNING
        VALUE(rval) TYPE REF TO lcl_ixml_istream_xstring.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_ixml_ostream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_stream_factory.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_ostream.

ENDCLASS.


CLASS lcl_ixml_ostream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lcl_ixml_stream_factory.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_ostream.

ENDCLASS.


CLASS lcl_ixml_parser DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_parser.

    CLASS-METHODS create
      IMPORTING
        document       TYPE REF TO zif_excel_ixml_document
        istream        TYPE REF TO zif_excel_ixml_istream
        stream_factory TYPE REF TO zif_excel_ixml_stream_factory
      RETURNING
        VALUE(rval)    TYPE REF TO lcl_ixml_parser.

  PRIVATE SECTION.

    DATA document             TYPE REF TO lcl_ixml_document.
    DATA istream              TYPE REF TO lif_ixml_istream.
    DATA stream_factory       TYPE REF TO zif_excel_ixml_stream_factory.

ENDCLASS.


CLASS lcl_ixml_renderer DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_renderer.
ENDCLASS.


CLASS lcl_ixml_stream_factory DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream_factory.
ENDCLASS.


CLASS lcl_ixml_document IMPLEMENTATION.
  METHOD parse.
    DATA lo_reader                  TYPE REF TO if_sxml_reader.
    DATA lv_current_level           TYPE i.
    DATA lv_current_element_node_id TYPE tv_node_id.
    DATA lo_node                    TYPE REF TO if_sxml_node.
    DATA lo_parse_error             TYPE REF TO cx_sxml_parse_error.
    DATA lo_node_close              TYPE REF TO if_sxml_close_element.
    DATA lr_parse_element_level     TYPE REF TO ts_parse_element_level.
    DATA lo_node_open               TYPE REF TO if_sxml_open_element.
    DATA ls_node                    TYPE ts_node.
    DATA ls_element                 TYPE ts_element.
    DATA lr_namespace               TYPE REF TO ts_namespace.
    DATA ls_namespace               TYPE ts_namespace.
    DATA ls_parse_element_level     TYPE ts_parse_element_level.
    DATA lo_node_value              TYPE REF TO if_sxml_value_node.

    lo_reader = istream->sxml_reader.
    lv_current_level = 0.
    lv_current_element_node_id = 0.

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
          BREAK-POINT."should not happen in OO parsing

        WHEN lo_node->co_nt_element_close.
          lo_node_close ?= lo_node.
          DELETE parse_element_levels WHERE level = lv_current_level.
          lv_current_level = lv_current_level - 1.
          IF lv_current_level = 0.
            lv_current_element_node_id = 0.
          ELSE.
            READ TABLE parse_element_levels WITH TABLE KEY level = lv_current_level REFERENCE INTO lr_parse_element_level.
            ASSERT sy-subrc = 0.
            lv_current_element_node_id = lr_parse_element_level->node_id.
          ENDIF.

        WHEN lo_node->co_nt_element_open.
          lo_node_open ?= lo_node.

          ls_node-id        = lines( nodes ) + 1.
          ls_node-type      = c_node_type-element.
          ls_node-parent_id = lv_current_element_node_id.
          INSERT ls_node INTO TABLE nodes.

          ls_element-id   = ls_node-id.
          ls_element-name = lo_node_open->qname-name.
          IF lo_node_open->qname-namespace IS NOT INITIAL.
            READ TABLE namespaces WITH TABLE KEY by_uri COMPONENTS uri = lo_node_open->qname-namespace REFERENCE INTO lr_namespace.
            IF sy-subrc <> 0.
              ls_namespace-id  = lines( namespaces ) + 1.
              ls_namespace-uri = lo_node_open->qname-namespace.
              INSERT ls_namespace INTO TABLE namespaces REFERENCE INTO lr_namespace.
            ENDIF.
            ls_element-namespace_id = lr_namespace->id.
          ELSE.
            ls_element-namespace_id = 0.
          ENDIF.
          ls_element-attributes = lo_node_open->get_attributes( ).
          INSERT ls_element INTO TABLE elements.

          lv_current_level = lv_current_level + 1.
          ls_parse_element_level-level   = lv_current_level.
          ls_parse_element_level-node_id = ls_node-id.
          INSERT ls_parse_element_level INTO TABLE parse_element_levels.

          lv_current_element_node_id = ls_node-id.

        WHEN lo_node->co_nt_final.
          BREAK-POINT."should not happen

        WHEN lo_node->co_nt_initial.
          BREAK-POINT."should not happen

        WHEN lo_node->co_nt_value.
          lo_node_value ?= lo_node.
      ENDCASE.
    ENDDO.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~append_child.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~clone.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~create_iterator.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name_ns.
    data lr_element type ref to ts_element.
    data lv_namespace_id type tv_namespace_id.
    data lr_namespace type ref to ts_namespace.

    IF uri IS INITIAL.
    lv_namespace_id = 0.
ELSE.
    READ TABLE namespaces WITH TABLE KEY by_uri COMPONENTS uri = uri REFERENCE INTO lr_namespace.
    IF sy-subrc = 0.
      lv_namespace_id = lr_namespace->id.
    ELSE.
    lv_namespace_id = 0.
    ENDIF.
ENDIF.

    READ TABLE elements WITH TABLE KEY by_name COMPONENTS name = name
                                                          namespace_id = lv_namespace_id
                                                          REFERENCE INTO lr_element.
    if sy-subrc = 0.
      IF lr_element->object IS NOT BOUND.
        lr_element->object = lcl_ixml_element=>create(
document = me
node_id  = lr_element->id ).
      endif.
      rval = lr_element->object.
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_attributes.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_children.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name_ns.
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

  METHOD zif_excel_ixml_document~get_root_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_value.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~query_interface.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_namespace_prefix.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_standalone.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_value.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_element IMPLEMENTATION.
  METHOD create.
    rval = new lcl_ixml_element( ).
    rval->document = document.
    rval->node_id = node_id.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~append_child.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~clone.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~create_iterator.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~find_from_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~find_from_name_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_attributes.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute_node_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_children.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_elements_by_tag_name.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_elements_by_tag_name_ns.
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

  METHOD zif_excel_ixml_node~get_value.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~query_interface.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~remove_attribute_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~set_attribute.
  ENDMETHOD.

  METHOD zif_excel_ixml_element~set_attribute_ns.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_namespace_prefix.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_value.
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


CLASS lcl_ixml_ostream_string IMPLEMENTATION.

ENDCLASS.


CLASS lcl_ixml_ostream_xstring IMPLEMENTATION.

ENDCLASS.


CLASS lcl_ixml_parser IMPLEMENTATION.
  METHOD create.
    rval = NEW lcl_ixml_parser( ).
    rval->document       ?= document.
    rval->istream        ?= istream.
    rval->stream_factory  = stream_factory.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~add_strip_space_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~parse.
    rval = document->parse( istream ).
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_normalizing.
    IF is_normalizing = abap_true.
      istream->sxml_reader->set_option( if_sxml_reader=>co_opt_normalizing ).
    ELSE.
      istream->sxml_reader->set_option( if_sxml_reader=>co_opt_keep_whitespace ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_validating.
    ASSERT mode = zif_excel_ixml_parser=>co_no_validation.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_ixml_renderer IMPLEMENTATION.
  METHOD zif_excel_ixml_renderer~render.
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
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_xstring.
  ENDMETHOD.
ENDCLASS.
