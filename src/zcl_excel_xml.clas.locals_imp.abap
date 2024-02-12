*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS ltc_rewrite_xml_via_sxml DEFINITION DEFERRED.


CLASS lcx_unexpected DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.


CLASS lcl_bom_utf16_as_character DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    "! UTF-16 BOM corresponding to the system, either big endian or system endian
    CLASS-DATA system_value  TYPE c LENGTH 1.
    CLASS-DATA big_endian    TYPE c LENGTH 1.
    CLASS-DATA little_endian TYPE c LENGTH 1.
ENDCLASS.


INTERFACE lif_isxml_istream.
  INTERFACES zif_excel_xml_istream.
  DATA sxml_reader TYPE REF TO if_sxml_reader READ-ONLY.
ENDINTERFACE.


INTERFACE lif_isxml_ostream.
  INTERFACES zif_excel_xml_ostream.
  DATA sxml_writer TYPE REF TO if_sxml_writer READ-ONLY.
  DATA type        TYPE c LENGTH 1            READ-ONLY.
ENDINTERFACE.


CLASS lcl_isxml_unknown DEFINITION
    INHERITING FROM lcl_isxml_root_all
    CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_unknown.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_qname,
        prefix TYPE string,
        name   TYPE string,
      END OF ts_qname.

    DATA type TYPE lcl_isxml=>tv_node_type.

    CLASS-METHODS split_name_into_qname
      IMPORTING
        iv_name          TYPE string
        iv_prefix        TYPE string
      RETURNING
        VALUE(rs_result) TYPE ts_qname.

ENDCLASS.


CLASS lcl_isxml_node DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PROTECTED
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node.

  PROTECTED SECTION.

    DATA document         TYPE REF TO lcl_isxml_document.
    DATA parent           TYPE REF TO lcl_isxml_node.
    DATA previous_sibling TYPE REF TO lcl_isxml_node.
    DATA next_sibling     TYPE REF TO lcl_isxml_node.
    DATA first_child      TYPE REF TO lcl_isxml_node.
    "! Useful for performance to APPEND
    DATA last_child       TYPE REF TO lcl_isxml_node.

    "! Must be redefined in subclasses
    METHODS clone
      RETURNING
        VALUE(ro_result) TYPE REF TO lcl_isxml_node.

    "! Must be redefined in subclasses
    METHODS render
      IMPORTING
        io_sxml_writer    TYPE REF TO if_sxml_writer
        io_isxml_renderer TYPE REF TO lcl_isxml_renderer
      RETURNING
        VALUE(rv_rc)      TYPE i.

  PRIVATE SECTION.

    METHODS remove_node.

ENDCLASS.


CLASS lcl_isxml_attribute DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PROTECTED
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_attribute.

  PRIVATE SECTION.

    CLASS-METHODS create
      IMPORTING
        iv_prefix           TYPE string
        iv_name             TYPE string
        iv_value            TYPE string
        io_previous_attribute TYPE REF TO lcl_isxml_node
      RETURNING
        VALUE(ro_result)    TYPE REF TO lcl_isxml_attribute.

    DATA prefix TYPE string.
    DATA name   TYPE string.
    DATA value  TYPE string.

    CLASS-DATA: BEGIN OF debug,
                  name   TYPE string,
                  prefix TYPE string,
                END OF debug.
ENDCLASS.


CLASS lcl_isxml_character_data DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PROTECTED
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_character_data.

ENDCLASS.


CLASS lcl_isxml_document DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_document.

  PROTECTED SECTION.

    METHODS render REDEFINITION.

  PRIVATE SECTION.

    DATA declaration TYPE boolean                   VALUE abap_true.
    DATA encoding    TYPE REF TO lcl_isxml_encoding.
    DATA version     TYPE string                    VALUE '1.0'.
    DATA standalone  TYPE string.

    METHODS get_xml_header
      IMPORTING
        iv_encoding      TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_xml_header_as_string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_xml_header_as_xstring
      RETURNING
        VALUE(rv_result) TYPE xstring.

ENDCLASS.


CLASS lcl_isxml_element DEFINITION
    INHERITING FROM lcl_isxml_node
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_element.

    METHODS zif_excel_xml_element~get_name REDEFINITION.

  PROTECTED SECTION.

    METHODS clone REDEFINITION.
    METHODS render REDEFINITION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_attribute,
        position       TYPE i,
        prefix         TYPE string,
        name           TYPE string,
        value_if_xmlns TYPE string,
        object         TYPE REF TO lcl_isxml_attribute,
      END OF ts_attribute.
    TYPES tt_attribute TYPE SORTED TABLE OF ts_attribute WITH UNIQUE KEY name prefix
                        WITH UNIQUE SORTED KEY by_position COMPONENTS position
                        WITH UNIQUE SORTED KEY by_prefix_name COMPONENTS prefix name
                        WITH NON-UNIQUE SORTED KEY by_prefix_value_nsuri COMPONENTS prefix value_if_xmlns.
    TYPES tt_element   TYPE STANDARD TABLE OF REF TO lcl_isxml_element WITH DEFAULT KEY.

    DATA name       TYPE string.
    DATA prefix     TYPE string.
    DATA namespace  TYPE string.
    DATA attributes TYPE tt_attribute.

    METHODS append_attribute
      IMPORTING
        iv_prefix TYPE string
        iv_name   TYPE string
        iv_value  TYPE string.

    METHODS find_from_name_ns
      IMPORTING
        iv_depth         TYPE i DEFAULT 0
        iv_name          TYPE string
        iv_nsuri         TYPE string DEFAULT ''
      RETURNING
        VALUE(ro_result) TYPE REF TO lcl_isxml_element.

    METHODS find_from_name_ns_recursive
      IMPORTING
        iv_depth         TYPE i DEFAULT 0
        iv_name          TYPE string
        iv_nsuri         TYPE string DEFAULT ''
      RETURNING
        VALUE(ro_result) TYPE REF TO lcl_isxml_element.

    METHODS get_elements_by_tag_name_ns
      IMPORTING
        iv_name          TYPE string
        iv_nsuri         TYPE string DEFAULT ''
      RETURNING
        VALUE(rt_result) TYPE tt_element.

    METHODS get_namespace_prefix_by_uri
      IMPORTING
        iv_uri           TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_namespace_uri_by_prefix
      IMPORTING
        iv_prefix        TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.


CLASS lcl_isxml_encoding DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_encoding.

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
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_named_node_map.
    INTERFACES lif_isxml_all_friends.

  PRIVATE SECTION.

    DATA element TYPE REF TO lcl_isxml_element.

ENDCLASS.


CLASS lcl_isxml_node_collection DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node_collection.

  PRIVATE SECTION.

    DATA table_nodes TYPE TABLE OF REF TO lcl_isxml_node.

ENDCLASS.


CLASS lcl_isxml_node_iterator DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node_iterator.

  PRIVATE SECTION.

    "! Used to iterate on the attributes of one element
    DATA named_node_map  TYPE REF TO lcl_isxml_named_node_map.
    "! Used to iterate its tree of children
    DATA node            TYPE REF TO lcl_isxml_node.
    DATA node_list       TYPE REF TO lcl_isxml_node_list.
    DATA node_collection TYPE REF TO lcl_isxml_node_collection.
    "! Used to iterate on all kind of objects
    DATA position        TYPE i.
    "! Used to iterate on the tree of children of "node"
    DATA current_node    TYPE REF TO lcl_isxml_node.

ENDCLASS.


CLASS lcl_isxml_node_list DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node_list.

  PRIVATE SECTION.

    DATA table_nodes TYPE TABLE OF REF TO lcl_isxml_node.

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
        xstring     TYPE REF TO xstring
      RETURNING
        VALUE(rval) TYPE REF TO lcl_isxml_ostream_xstring.

ENDCLASS.


CLASS lcl_isxml_parser DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_parser.

  PRIVATE SECTION.

    DATA document                TYPE REF TO lcl_isxml_document.
    DATA istream                 TYPE REF TO lif_isxml_istream.
    DATA stream_factory          TYPE REF TO zif_excel_xml_stream_factory.
    DATA add_strip_space_element TYPE abap_bool                           VALUE abap_false.
    DATA normalizing             TYPE abap_bool                           VALUE abap_true.

ENDCLASS.


CLASS lcl_isxml_renderer DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_renderer.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_namespace,
        level     TYPE i,
        "! Negative number used in index BY_PREFIX, to order eponymous prefixes last level first.
        neg_level TYPE i,
        prefix    TYPE string,
        uri       TYPE string,
      END OF ts_namespace.
    TYPES tt_namespace TYPE STANDARD TABLE OF ts_namespace WITH DEFAULT KEY
                    WITH UNIQUE SORTED KEY by_level_prefix COMPONENTS level prefix
                    WITH UNIQUE SORTED KEY by_prefix COMPONENTS prefix neg_level.
    TYPES:
      BEGIN OF ts_element_traced,
        element TYPE REF TO lcl_isxml_element,
        name    TYPE string,
        level   TYPE i,
      END OF ts_element_traced.

    DATA document           TYPE REF TO lcl_isxml_document.
    DATA ostream            TYPE REF TO lif_isxml_ostream.
    DATA current_level      TYPE i.
    DATA current_namespaces TYPE tt_namespace.
    CLASS-DATA trace_active TYPE abap_bool.
    DATA elements_processed TYPE HASHED TABLE OF REF TO lcl_isxml_element WITH UNIQUE KEY table_line.
    DATA elements_traced    TYPE STANDARD TABLE OF ts_element_traced WITH DEFAULT KEY.

ENDCLASS.


CLASS lcl_isxml_stream DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_stream.
ENDCLASS.


CLASS lcl_isxml_stream_factory DEFINITION
    INHERITING FROM lcl_isxml_unknown
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_stream_factory.
ENDCLASS.


CLASS lcl_isxml_text DEFINITION
    INHERITING FROM lcl_isxml_character_data
    CREATE PRIVATE
    FRIENDS lif_isxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_text.

  PROTECTED SECTION.

    METHODS render REDEFINITION.

  PRIVATE SECTION.

    DATA value TYPE string.

ENDCLASS.


CLASS lcl_rewrite_xml_via_sxml DEFINITION
    CREATE PRIVATE
    FRIENDS zcl_excel_xml
            ltc_rewrite_xml_via_sxml.

  PUBLIC SECTION.

    CLASS-METHODS execute
      IMPORTING
        iv_xml_string TYPE string
        iv_trace      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_string) TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_attribute,
        name      TYPE string,
        namespace TYPE string,
        prefix    TYPE string,
      END OF ts_attribute.
    TYPES tt_attribute TYPE STANDARD TABLE OF ts_attribute WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ts_element,
        name      TYPE string,
        namespace TYPE string,
        prefix    TYPE string,
      END OF ts_element.
    TYPES:
      BEGIN OF ts_nsbinding,
        prefix TYPE string,
        nsuri  TYPE string,
      END OF ts_nsbinding.
    TYPES tt_nsbinding TYPE STANDARD TABLE OF ts_nsbinding WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ts_complete_element,
        element    TYPE ts_element,
        attributes TYPE tt_attribute,
        nsbindings TYPE tt_nsbinding,
      END OF ts_complete_element.

    CLASS-DATA complete_parsed_elements TYPE TABLE OF ts_complete_element.

ENDCLASS.


INTERFACE lif_wrap_ixml_all_friends.
ENDINTERFACE.


INTERFACE lif_wrap_ixml_istream.
  INTERFACES zif_excel_xml_istream.
  DATA ixml_istream TYPE REF TO if_ixml_istream.
ENDINTERFACE.


INTERFACE lif_wrap_ixml_ostream.
  INTERFACES zif_excel_xml_ostream.
  DATA ixml_ostream TYPE REF TO if_ixml_ostream.
ENDINTERFACE.


CLASS lth_wrap_ixml_unknown DEFINITION
*    FOR TESTING
    CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_unknown.
    INTERFACES lif_wrap_ixml_all_friends.

ENDCLASS.


CLASS lth_wrap_ixml_node DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PROTECTED
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node.

  PRIVATE SECTION.

    DATA ixml_node TYPE REF TO if_ixml_node.

ENDCLASS.


CLASS lth_wrap_ixml DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml.

    TYPES:
      BEGIN OF ts_wrapped_ixml_object,
        ixml_object         TYPE REF TO object,
        ixml_object_wrapper TYPE REF TO object,
      END OF ts_wrapped_ixml_object.
    TYPES tt_wrapped_ixml_object TYPE HASHED TABLE OF ts_wrapped_ixml_object WITH UNIQUE KEY ixml_object
                                WITH UNIQUE HASHED KEY by_wrapper COMPONENTS ixml_object_wrapper.

    CLASS-DATA wrapped_ixml_objects TYPE tt_wrapped_ixml_object.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_excel_xml
      RAISING
        cx_sy_dyn_call_illegal_class.

    CLASS-METHODS unwrap_ixml
      IMPORTING
        io_wrap_ixml_unknown TYPE REF TO lth_wrap_ixml_unknown
      RETURNING
        VALUE(ro_result)     TYPE REF TO object.

    CLASS-METHODS wrap_ixml
      IMPORTING
        io_ixml_unknown  TYPE REF TO if_ixml_unknown
      RETURNING
        VALUE(ro_result) TYPE REF TO object.

  PRIVATE SECTION.

    CLASS-DATA singleton TYPE REF TO lth_wrap_ixml.
    DATA ixml TYPE REF TO if_ixml.

ENDCLASS.


CLASS lth_wrap_ixml_attribute DEFINITION
    INHERITING FROM lth_wrap_ixml_node
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_attribute.

  PRIVATE SECTION.

    DATA ixml_attribute TYPE REF TO if_ixml_attribute.

ENDCLASS.


CLASS lth_wrap_ixml_character_data DEFINITION
    INHERITING FROM lth_wrap_ixml_node
*    FOR TESTING
    CREATE PROTECTED
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_character_data.

ENDCLASS.


CLASS lth_wrap_ixml_document DEFINITION
    INHERITING FROM lth_wrap_ixml_node
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_document.

  PRIVATE SECTION.

    DATA ixml_document TYPE REF TO if_ixml_document.

ENDCLASS.


CLASS lth_wrap_ixml_element DEFINITION
    INHERITING FROM lth_wrap_ixml_node
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_element.

  PRIVATE SECTION.

    DATA ixml_element TYPE REF TO if_ixml_element.

ENDCLASS.


CLASS lth_wrap_ixml_encoding DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_encoding.

  PRIVATE SECTION.

    DATA ixml_encoding TYPE REF TO if_ixml_encoding.

ENDCLASS.


CLASS lth_wrap_ixml_istream_string DEFINITION
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_istream.

ENDCLASS.


CLASS lth_wrap_ixml_istream_xstring DEFINITION
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_istream.

ENDCLASS.


CLASS lth_wrap_ixml_named_node_map DEFINITION
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_named_node_map.

  PRIVATE SECTION.

    DATA ixml_named_node_map TYPE REF TO if_ixml_named_node_map.

ENDCLASS.


CLASS lth_wrap_ixml_node_collection DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node_collection.

  PRIVATE SECTION.

    DATA ixml_node_collection TYPE REF TO if_ixml_node_collection.

ENDCLASS.


CLASS lth_wrap_ixml_node_iterator DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node_iterator.

  PRIVATE SECTION.

    DATA ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

ENDCLASS.


CLASS lth_wrap_ixml_node_list DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_node_list.

  PRIVATE SECTION.

    DATA ixml_node_list TYPE REF TO if_ixml_node_list.

ENDCLASS.


CLASS lth_wrap_ixml_ostream_string DEFINITION
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_ostream.

ENDCLASS.


CLASS lth_wrap_ixml_ostream_xstring DEFINITION
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_ostream.

ENDCLASS.


CLASS lth_wrap_ixml_parser DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_parser.

  PRIVATE SECTION.

    DATA ixml_parser TYPE REF TO if_ixml_parser.

ENDCLASS.


CLASS lth_wrap_ixml_renderer DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_renderer.

  PRIVATE SECTION.

    DATA ixml_renderer TYPE REF TO if_ixml_renderer.

ENDCLASS.


CLASS lth_wrap_ixml_stream DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_stream.

ENDCLASS.


CLASS lth_wrap_ixml_stream_factory DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_stream_factory.

  PRIVATE SECTION.

    DATA ixml_stream_factory TYPE REF TO if_ixml_stream_factory.

ENDCLASS.


CLASS lth_wrap_ixml_text DEFINITION
    INHERITING FROM lth_wrap_ixml_character_data
*    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_xml_text.

  PRIVATE SECTION.

    DATA ixml_text TYPE REF TO if_ixml_text.

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
  METHOD zif_excel_xml~create_document.
    DATA lo_document TYPE REF TO lcl_isxml_document.

    CREATE OBJECT lo_document.
    lo_document->type     = zif_excel_xml_node=>co_node_document.
    lo_document->document = lo_document.
    rval = lo_document.
  ENDMETHOD.

  METHOD zif_excel_xml~create_encoding.
    DATA lo_encoding TYPE REF TO lcl_isxml_encoding.

    CREATE OBJECT lo_encoding.
    lo_encoding->byte_order    = byte_order.
    lo_encoding->character_set = character_set.
    rval = lo_encoding.
  ENDMETHOD.

  METHOD zif_excel_xml~create_parser.
    DATA lo_parser TYPE REF TO lcl_isxml_parser.

    CREATE OBJECT lo_parser.
    lo_parser->document       ?= document.
    lo_parser->istream        ?= istream.
    lo_parser->stream_factory  = stream_factory.
    rval = lo_parser.
  ENDMETHOD.

  METHOD zif_excel_xml~create_renderer.
    DATA lo_renderer TYPE REF TO lcl_isxml_renderer.

    CREATE OBJECT lo_renderer.
    lo_renderer->document ?= document.
    lo_renderer->ostream  ?= ostream.
    rval = lo_renderer.
  ENDMETHOD.

  METHOD zif_excel_xml~create_stream_factory.
    DATA lo_isxml_stream_factory TYPE REF TO lcl_isxml_stream_factory.

    CREATE OBJECT lo_isxml_stream_factory.
    rval = lo_isxml_stream_factory.
  ENDMETHOD.

  METHOD get_singleton.
    IF singleton IS NOT BOUND.
      CREATE OBJECT singleton TYPE lcl_isxml.
      IF 0 = 1.
        " Debug helper to use IXML instead (NB: cannot work with ABAP Cloud)
        singleton = lth_wrap_ixml=>create( ).
      ENDIF.
    ENDIF.
    rval = singleton.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_attribute IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT ro_result.
    ro_result->type             = zif_excel_xml_node=>co_node_attribute.
    ro_result->prefix           = iv_prefix.
    ro_result->name             = iv_name.
    ro_result->value            = iv_value.
    ro_result->previous_sibling = io_previous_attribute.
    IF     iv_name   = debug-name
       AND iv_prefix = debug-prefix.
      ASSERT 1 = 1. " debug helper
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_character_data IMPLEMENTATION.

ENDCLASS.


CLASS lcl_isxml_document IMPLEMENTATION.
  METHOD get_xml_header.
    DATA lv_string TYPE string.
    DATA lt_string TYPE TABLE OF string.

    CHECK declaration = abap_true.

    IF version IS NOT INITIAL.
      lv_string = |version="{ version }"|.
      INSERT lv_string INTO TABLE lt_string.
    ENDIF.

    IF iv_encoding IS NOT INITIAL.
      lv_string = |encoding="{ iv_encoding }"|.
      INSERT lv_string INTO TABLE lt_string.
    ENDIF.

    IF standalone IS NOT INITIAL.
      lv_string = |standalone="{ standalone }"|.
      INSERT lv_string INTO TABLE lt_string.
    ENDIF.

    rv_result = |<?xml { concat_lines_of( table = lt_string
                                          sep   = ` ` ) }?>|.
  ENDMETHOD.

  METHOD get_xml_header_as_string.
    rv_result = |{ lcl_bom_utf16_as_character=>system_value }{ get_xml_header( 'utf-16' ) }|.
  ENDMETHOD.

  METHOD get_xml_header_as_xstring.
    DATA lv_character_set TYPE string.

    IF encoding IS BOUND.
      lv_character_set = encoding->character_set.
    ELSE.
      lv_character_set = ''.
    ENDIF.
    rv_result = cl_abap_codepage=>convert_to( get_xml_header( to_lower( lv_character_set ) ) ).
  ENDMETHOD.

  METHOD render.
  ENDMETHOD.

  METHOD zif_excel_xml_document~create_element.
    DATA lo_element TYPE REF TO lcl_isxml_element.

    CREATE OBJECT lo_element.
    lo_element->type = zif_excel_xml_node=>co_node_element.
    lo_element->name = name.
    rval = lo_element.
  ENDMETHOD.

  METHOD zif_excel_xml_document~create_simple_element.
    rval = zif_excel_xml_document~create_simple_element_ns( name   = name
                                                            parent = parent ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~create_simple_element_ns.
    DATA ls_qname   TYPE ts_qname.
    DATA lo_element TYPE REF TO lcl_isxml_element.

    ls_qname = split_name_into_qname( iv_name   = name
                                      iv_prefix = prefix ).

    CREATE OBJECT lo_element.
    lo_element->type   = zif_excel_xml_node=>co_node_element.
    lo_element->name   = ls_qname-name.
    lo_element->prefix = ls_qname-prefix.

    parent->append_child( lo_element ).

    rval = lo_element.
  ENDMETHOD.

  METHOD zif_excel_xml_document~find_from_name.
    rval = zif_excel_xml_document~find_from_name_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~find_from_name_ns.
    DATA lo_isxml_element TYPE REF TO lcl_isxml_element.

    IF first_child is not bound.
      RETURN.
    endif.
    lo_isxml_element ?= first_child.
    rval = lo_isxml_element->find_from_name_ns( iv_depth = -1
                                                iv_name  = name
                                                iv_nsuri = uri ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~get_elements_by_tag_name.
    rval = zif_excel_xml_document~get_elements_by_tag_name_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~get_elements_by_tag_name_ns.
    DATA lo_isxml_element         TYPE REF TO lcl_isxml_element.
    DATA lt_element               TYPE lcl_isxml_element=>tt_element.
    DATA lo_isxml_node_collection TYPE REF TO lcl_isxml_node_collection.
    DATA lo_element               TYPE REF TO lcl_isxml_element.

    lo_isxml_element ?= first_child.
    lt_element = lo_isxml_element->get_elements_by_tag_name_ns( iv_name  = name
                                                                iv_nsuri = uri ).
    CREATE OBJECT lo_isxml_node_collection.
    LOOP AT lt_element INTO lo_element.
      INSERT lo_element INTO TABLE lo_isxml_node_collection->table_nodes.
    ENDLOOP.
    rval = lo_isxml_node_collection.
  ENDMETHOD.

  METHOD zif_excel_xml_document~get_root_element.
    rval ?= first_child.
  ENDMETHOD.

  METHOD zif_excel_xml_document~set_declaration.
    me->declaration = declaration.
  ENDMETHOD.

  METHOD zif_excel_xml_document~set_encoding.
    me->encoding ?= encoding.
  ENDMETHOD.

  METHOD zif_excel_xml_document~set_standalone.
    IF standalone = abap_true.
      me->standalone = 'yes'.
    ELSE.
      me->standalone = ''.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_element IMPLEMENTATION.
  METHOD clone.
    DATA lo_isxml_element TYPE REF TO lcl_isxml_element.

    CREATE OBJECT lo_isxml_element.
    lo_isxml_element->attributes = attributes.
    lo_isxml_element->name       = name.
    lo_isxml_element->namespace  = namespace.
    lo_isxml_element->prefix     = prefix.
    lo_isxml_element->type       = type.

    ro_result = lo_isxml_element.
  ENDMETHOD.

  METHOD find_from_name_ns.
    IF     name      = iv_name
       AND namespace = iv_nsuri.
      ro_result = me.
      RETURN.
    ENDIF.

    ro_result = find_from_name_ns_recursive( iv_depth = iv_depth
                                             iv_name  = iv_name
                                             iv_nsuri = iv_nsuri ).
  ENDMETHOD.

  METHOD find_from_name_ns_recursive.
    DATA lv_depth         TYPE i.
    DATA lo_child         TYPE REF TO lcl_isxml_node.
    DATA lo_isxml_element TYPE REF TO lcl_isxml_element.

    IF iv_depth = 0.
      lv_depth = 0.
    ELSE.
      lv_depth = iv_depth - 1.
    ENDIF.

    lo_child = first_child.
    WHILE lo_child IS BOUND.
      IF lo_child->type = if_ixml_node=>co_node_element.
        lo_isxml_element ?= lo_child.

        IF     lo_isxml_element->name      = iv_name
           AND lo_isxml_element->namespace = iv_nsuri.
          ro_result = lo_isxml_element.
          RETURN.
        ENDIF.
        IF iv_depth <> 1.
          lo_isxml_element = lo_isxml_element->find_from_name_ns_recursive( iv_depth = lv_depth
                                                                            iv_name  = iv_name
                                                                            iv_nsuri = iv_nsuri ).
          IF lo_isxml_element IS BOUND.
            ro_result = lo_isxml_element.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
      lo_child = lo_child->next_sibling.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_elements_by_tag_name_ns.
    DATA lo_child         TYPE REF TO lcl_isxml_node.
    DATA lo_isxml_element TYPE REF TO lcl_isxml_element.
    DATA lt_element       TYPE tt_element.

    IF     name      = iv_name
       AND namespace = iv_nsuri.
      INSERT me INTO TABLE rt_result.
    ENDIF.

    lo_child = first_child.
    WHILE lo_child IS BOUND.
      IF lo_child->type = if_ixml_node=>co_node_element.
        lo_isxml_element ?= lo_child.
        lt_element = lo_isxml_element->get_elements_by_tag_name_ns( iv_name  = iv_name
                                                                    iv_nsuri = iv_nsuri ).
        INSERT LINES OF lt_element INTO TABLE rt_result.
      ENDIF.
      lo_child = lo_child->next_sibling.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_namespace_prefix_by_uri.
    DATA lo_isxml_element TYPE REF TO lcl_isxml_element.
    DATA lr_attribute     TYPE REF TO lcl_isxml_element=>ts_attribute.

    IF prefix = 'http://www.w3.org/XML/1998/namespace'.
      rv_result = 'xml'.
      RETURN.
    ENDIF.

    lo_isxml_element = me.
    DO.
      READ TABLE lo_isxml_element->attributes
           WITH KEY by_prefix_value_nsuri
           COMPONENTS prefix         = 'xmlns'
                      value_if_xmlns = iv_uri
           REFERENCE INTO lr_attribute.
      IF sy-subrc = 0.
        rv_result = lr_attribute->name.
        EXIT.
      ELSEIF    lo_isxml_element->parent  = lo_isxml_element->document
             OR lo_isxml_element->parent IS NOT BOUND.
        EXIT.
      ENDIF.
      lo_isxml_element ?= lo_isxml_element->parent.
    ENDDO.
  ENDMETHOD.

  METHOD get_namespace_uri_by_prefix.
    DATA lo_isxml_element TYPE REF TO lcl_isxml_element.
    DATA lr_attribute     TYPE REF TO lcl_isxml_element=>ts_attribute.

    IF prefix = 'xml'.
      rv_result = 'http://www.w3.org/XML/1998/namespace'.
      RETURN.
    ENDIF.

    lo_isxml_element = me.
    DO.
      READ TABLE lo_isxml_element->attributes
           WITH KEY by_prefix_name
           COMPONENTS prefix = 'xmlns'
                      name   = iv_prefix
           REFERENCE INTO lr_attribute.
      IF sy-subrc = 0.
        rv_result = lr_attribute->value_if_xmlns.
        EXIT.
      ELSEIF    lo_isxml_element->parent  = lo_isxml_element->document
             OR lo_isxml_element->parent IS NOT BOUND.
        EXIT.
      ENDIF.
      lo_isxml_element ?= lo_isxml_element->parent.
    ENDDO.
  ENDMETHOD.

  METHOD render.
    TYPES:
      BEGIN OF ts_namespace_declaration,
        nsprefix TYPE string,
        nsuri    TYPE string,
      END OF ts_namespace_declaration.

    DATA lv_previous_level    TYPE lcl_isxml_renderer=>ts_namespace-level.
    DATA ls_namespace         TYPE lcl_isxml_renderer=>ts_namespace.
    DATA lo_sxml_open_element TYPE REF TO if_sxml_open_element.
    DATA lr_namespace         TYPE REF TO lcl_isxml_renderer=>ts_namespace.
    DATA lv_nsuri             TYPE string.
    DATA lr_isxml_attribute   TYPE REF TO lcl_isxml_element=>ts_attribute.
    DATA lo_sxml_name_error   TYPE REF TO cx_sxml_name_error.
    DATA lo_isxml_child_node  TYPE REF TO lcl_isxml_node.
    DATA lo_excel_xml_error   TYPE REF TO cx_sxml_name_error.

    FIELD-SYMBOLS <ls_attribute> TYPE lcl_isxml_element=>ts_attribute.

    IF io_isxml_renderer->trace_active = abap_true.
      READ TABLE io_isxml_renderer->elements_processed WITH TABLE KEY table_line = me TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Should never happen = endless loop is happening
        ASSERT 1 = 1. " debug helper
      ENDIF.
      INSERT me INTO TABLE io_isxml_renderer->elements_processed.
      INSERT VALUE #( element = me
                      name    = name
                      level   = io_isxml_renderer->current_level )
             INTO TABLE io_isxml_renderer->elements_traced.
    ENDIF.

    TRY.

        "==============
        " Namespaces
        "==============
        lv_previous_level = io_isxml_renderer->current_level.
        io_isxml_renderer->current_level = io_isxml_renderer->current_level + 1.

        "   1. Add namespaces with a prefix
        LOOP AT attributes ASSIGNING <ls_attribute>
             USING KEY by_prefix_name
             WHERE prefix = 'xmlns'.
          ls_namespace-level     = io_isxml_renderer->current_level.
          ls_namespace-neg_level = -1 * io_isxml_renderer->current_level.
          ls_namespace-prefix    = <ls_attribute>-object->name.
          ls_namespace-uri       = <ls_attribute>-object->value.
          INSERT ls_namespace INTO TABLE io_isxml_renderer->current_namespaces.
        ENDLOOP.

        "   2. Add default namespace
        READ TABLE attributes ASSIGNING <ls_attribute>
             WITH KEY by_prefix_name
             COMPONENTS prefix = ''
                        name   = 'xmlns'.
        IF sy-subrc = 0.
          ls_namespace-level     = io_isxml_renderer->current_level.
          ls_namespace-neg_level = -1 * io_isxml_renderer->current_level.
          ls_namespace-prefix    = ''.
          ls_namespace-uri       = <ls_attribute>-object->value.
          INSERT ls_namespace INTO TABLE io_isxml_renderer->current_namespaces.
        ENDIF.

        "==============
        " Add element
        "==============
        IF prefix IS INITIAL.
          lo_sxml_open_element = io_sxml_writer->new_open_element( name = name ).
        ELSE.
          READ TABLE io_isxml_renderer->current_namespaces
               WITH KEY by_prefix
               COMPONENTS prefix = prefix
               REFERENCE INTO lr_namespace.
          IF sy-subrc = 0.
            lv_nsuri = lr_namespace->uri.
          ELSE.
            RAISE EXCEPTION TYPE lcx_unexpected.
          ENDIF.
          lo_sxml_open_element = io_sxml_writer->new_open_element( name   = name
                                                                   nsuri  = lv_nsuri
                                                                   prefix = prefix ).
        ENDIF.

        "==============
        " Element attributes
        "==============
        LOOP AT attributes REFERENCE INTO lr_isxml_attribute
             USING KEY by_position.

          IF    lr_isxml_attribute->prefix = 'xmlns'
             OR lr_isxml_attribute->name   = 'xmlns'.
            CONTINUE.
          ENDIF.

          TRY.
              CLEAR lv_nsuri.
              IF lr_isxml_attribute->prefix IS NOT INITIAL.
                IF lr_isxml_attribute->prefix = 'xml'.
                  lv_nsuri = 'http://www.w3.org/XML/1998/namespace'.
                ELSE.
                  READ TABLE io_isxml_renderer->current_namespaces
                       WITH KEY by_prefix
                       COMPONENTS prefix = lr_isxml_attribute->prefix
                       REFERENCE INTO lr_namespace.
                  IF sy-subrc = 0.
                    lv_nsuri = lr_namespace->uri.
                  ENDIF.
                ENDIF.
              ENDIF.
              lo_sxml_open_element->set_attribute( name   = lr_isxml_attribute->object->name
                                                   nsuri  = lv_nsuri
                                                   prefix = lr_isxml_attribute->object->prefix
                                                   value  = lr_isxml_attribute->object->value ).

            CATCH cx_sxml_name_error INTO lo_sxml_name_error.
              RAISE EXCEPTION TYPE zcx_excel_xml
                EXPORTING previous = lo_sxml_name_error.
          ENDTRY.
        ENDLOOP.

        "==============
        " Write the element defaukt namespace declaration xmlns="..."
        "==============
        READ TABLE io_isxml_renderer->current_namespaces
             REFERENCE INTO lr_namespace
             WITH TABLE KEY by_level_prefix
             COMPONENTS level  = io_isxml_renderer->current_level
                        prefix = ''.
        IF sy-subrc = 0.
          lo_sxml_open_element->set_attribute( name  = 'xmlns'
                                               value = lr_namespace->uri ).
        ENDIF.

        "==============
        " Write element
        "==============
        io_sxml_writer->write_node( lo_sxml_open_element ).

        "==============
        " Write the element namespace declarations xmlns:xxx="..." which are not used by any attribute
        " (must be done after the element has been written)
        "==============
        LOOP AT io_isxml_renderer->current_namespaces
             REFERENCE INTO lr_namespace
             USING KEY by_level_prefix
             WHERE     level   = io_isxml_renderer->current_level
                   AND prefix IS NOT INITIAL.
          READ TABLE attributes TRANSPORTING NO FIELDS
               WITH KEY by_prefix_name
               COMPONENTS prefix = lr_namespace->prefix.
          IF sy-subrc <> 0.
            io_sxml_writer->write_namespace_declaration( nsuri  = lr_namespace->uri
                                                         prefix = lr_namespace->prefix ).
          ENDIF.
        ENDLOOP.

        "==============
        " Process element child nodes
        "==============
        lo_isxml_child_node = first_child.
        WHILE lo_isxml_child_node IS BOUND.
          lo_isxml_child_node->render( io_sxml_writer    = io_sxml_writer
                                       io_isxml_renderer = io_isxml_renderer ).
          lo_isxml_child_node = lo_isxml_child_node->next_sibling.
        ENDWHILE.

        "==============
        " End of element
        "==============
        io_sxml_writer->close_element( ).

        DELETE io_isxml_renderer->current_namespaces
               USING KEY by_level_prefix
               WHERE level = io_isxml_renderer->current_level.
        io_isxml_renderer->current_level = io_isxml_renderer->current_level - 1.

        rv_rc = 0.

      CATCH zcx_excel_xml.
        rv_rc = lcl_isxml=>ixml_mr-renderer_error.
      CATCH cx_sxml_name_error INTO lo_sxml_name_error.
        " Instantiate only for helping debug
        CREATE OBJECT lo_excel_xml_error
          EXPORTING previous = lo_sxml_name_error.
        rv_rc = lcl_isxml=>ixml_mr-renderer_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_excel_xml_element~find_from_name.
    rval = zif_excel_xml_element~find_from_name_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~find_from_name_ns.
    rval = find_from_name_ns( iv_depth = depth
                              iv_name  = name
                              iv_nsuri = uri ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_attribute.
    rval = zif_excel_xml_element~get_attribute_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_attribute_node_ns.
    DATA lv_nsprefix  TYPE string.
    DATA lr_attribute TYPE REF TO ts_attribute.

    IF uri IS NOT INITIAL.
      lv_nsprefix = get_namespace_prefix_by_uri( uri ).
    ENDIF.
    LOOP AT attributes REFERENCE INTO lr_attribute
         USING KEY primary_key
         WHERE     name   = name
               AND prefix = lv_nsprefix.
      rval = lr_attribute->object.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_attribute_ns.
    DATA lo_isxml_attribute TYPE REF TO lcl_isxml_attribute.

    lo_isxml_attribute ?= zif_excel_xml_element~get_attribute_node_ns( name = name
                                                                       uri  = uri ).
    IF lo_isxml_attribute IS BOUND.
      rval = lo_isxml_attribute->value.
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_elements_by_tag_name.
    rval = zif_excel_xml_element~get_elements_by_tag_name_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_elements_by_tag_name_ns.
    DATA lt_element               TYPE lcl_isxml_element=>tt_element.
    DATA lo_isxml_node_collection TYPE REF TO lcl_isxml_node_collection.
    DATA lo_element               TYPE REF TO lcl_isxml_element.

    lt_element = get_elements_by_tag_name_ns( iv_name  = name
                                              iv_nsuri = uri ).
    CREATE OBJECT lo_isxml_node_collection.
    LOOP AT lt_element INTO lo_element.
      INSERT lo_element INTO TABLE lo_isxml_node_collection->table_nodes.
    ENDLOOP.
    rval = lo_isxml_node_collection.
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_name.
    rval = name.
  ENDMETHOD.

  METHOD zif_excel_xml_element~remove_attribute_ns.
    DELETE attributes
           USING KEY primary_key
           WHERE     name   = name
                 AND prefix = space.
  ENDMETHOD.

  METHOD zif_excel_xml_element~set_attribute.
    zif_excel_xml_element~set_attribute_ns( name   = name
                                            prefix = namespace
                                            value  = value ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~set_attribute_ns.
    DATA ls_qname TYPE ts_qname.

    ls_qname = split_name_into_qname( iv_name   = name
                                      iv_prefix = prefix ).
    append_attribute( iv_prefix = ls_qname-prefix
                      iv_name   = ls_qname-name
                      iv_value  = value ).
  ENDMETHOD.

  METHOD append_attribute.
    DATA lv_index_last_attribute TYPE i.
    DATA lr_isxml_attribute      TYPE REF TO ts_attribute.
    DATA lo_previous_attribute   TYPE REF TO lcl_isxml_attribute.
    DATA ls_isxml_attribute      TYPE ts_attribute.

    lv_index_last_attribute = lines( attributes ).
    READ TABLE attributes INDEX lv_index_last_attribute REFERENCE INTO lr_isxml_attribute.
    IF sy-subrc = 0.
      lo_previous_attribute = lr_isxml_attribute->object.
    ENDIF.

    ls_isxml_attribute-position = lines( attributes ) + 1.
    ls_isxml_attribute-prefix   = iv_prefix.
    ls_isxml_attribute-name     = iv_name.
    IF    iv_prefix = 'xmlns'
       OR iv_name   = 'xmlns'.
      ls_isxml_attribute-value_if_xmlns = iv_value.
    ENDIF.
    ls_isxml_attribute-object = lcl_isxml_attribute=>create( iv_prefix             = iv_prefix
                                                             iv_name               = iv_name
                                                             iv_value              = iv_value
                                                             io_previous_attribute = lo_previous_attribute ).
    INSERT ls_isxml_attribute INTO TABLE attributes.

    IF lo_previous_attribute IS BOUND.
      lo_previous_attribute->next_sibling = ls_isxml_attribute-object.
    ENDIF.
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

  METHOD zif_excel_xml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_istream_xstring IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT rval.
    rval->lif_isxml_istream~sxml_reader = cl_sxml_string_reader=>create( input = string ).
  ENDMETHOD.

  METHOD zif_excel_xml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_named_node_map IMPLEMENTATION.
  METHOD zif_excel_xml_named_node_map~create_iterator.
    DATA lo_isxml_node_iterator TYPE REF TO lcl_isxml_node_iterator.

    CREATE OBJECT lo_isxml_node_iterator.
    lo_isxml_node_iterator->named_node_map = me.

    rval = lo_isxml_node_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node IMPLEMENTATION.
  METHOD clone.
    " Must be redefined in subclasses.
    RAISE EXCEPTION TYPE lcx_unexpected.
  ENDMETHOD.

  METHOD remove_node.
    IF previous_sibling IS BOUND.
      previous_sibling->next_sibling = next_sibling.
    ENDIF.
    IF next_sibling IS BOUND.
      next_sibling->previous_sibling = previous_sibling.
    ENDIF.
    IF parent IS BOUND.
      IF parent->first_child = me.
        parent->first_child = next_sibling.
      ENDIF.
      IF parent->last_child = me.
        parent->last_child = previous_sibling.
      ENDIF.
    ENDIF.
    CLEAR document.
    CLEAR parent.
    CLEAR previous_sibling.
    CLEAR next_sibling.
  ENDMETHOD.

  METHOD render.
    " Must be redefined in subclasses.
    RAISE EXCEPTION TYPE lcx_unexpected.
  ENDMETHOD.

  METHOD zif_excel_xml_node~append_child.
    DATA cast_new_child TYPE REF TO lcl_isxml_node.

    IF new_child IS NOT BOUND.
      "rval = lcl_isxml=>ixml_mr-dom_invalid_arg.
      RETURN.
    ENDIF.

    cast_new_child ?= new_child.
    cast_new_child->remove_node( ).

    IF first_child IS NOT BOUND.
      first_child ?= new_child.
    ENDIF.

    IF last_child IS BOUND.
      last_child->next_sibling ?= new_child.
    ENDIF.

    cast_new_child->document         = document.
    cast_new_child->parent           = me.
    cast_new_child->previous_sibling = last_child.
    cast_new_child->next_sibling     = lcl_isxml=>no_node.

    last_child ?= new_child.
  ENDMETHOD.

  METHOD zif_excel_xml_node~clone.
    rval = clone( ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~create_iterator.
    DATA lo_isxml_node_iterator TYPE REF TO lcl_isxml_node_iterator.

    CREATE OBJECT lo_isxml_node_iterator.
    lo_isxml_node_iterator->node = me.

    rval = lo_isxml_node_iterator.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_attributes.
    DATA lo_isxml_named_node_map TYPE REF TO lcl_isxml_named_node_map.

    CHECK type = zif_excel_xml_node=>co_node_element.

    CREATE OBJECT lo_isxml_named_node_map.
    lo_isxml_named_node_map->element ?= me.
    rval = lo_isxml_named_node_map.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_children.
    DATA lo_isxml_node_list TYPE REF TO lcl_isxml_node_list.
    DATA lo_child           TYPE REF TO lcl_isxml_node.

    CREATE OBJECT lo_isxml_node_list.

    lo_child = first_child.
    WHILE lo_child IS BOUND.
      INSERT lo_child INTO TABLE lo_isxml_node_list->table_nodes.
      lo_child = lo_child->next_sibling.
    ENDWHILE.

    rval = lo_isxml_node_list.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_first_child.
    rval = first_child.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_name.
    DATA lo_isxml_attribute TYPE REF TO lcl_isxml_attribute.

    CASE type.
      WHEN zif_excel_xml_node=>co_node_attribute.
        lo_isxml_attribute ?= me.
        rval = lo_isxml_attribute->name.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_namespace_prefix.
    DATA lo_isxml_attribute TYPE REF TO lcl_isxml_attribute.
    DATA lo_isxml_element   TYPE REF TO lcl_isxml_element.

    CASE type.
      WHEN zif_excel_xml_node=>co_node_attribute.
        lo_isxml_attribute ?= me.
        rval = lo_isxml_attribute->prefix.
      WHEN zif_excel_xml_node=>co_node_element.
        lo_isxml_element ?= me.
        rval = lo_isxml_element->prefix.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_namespace_uri.
    DATA lo_isxml_attribute TYPE REF TO lcl_isxml_attribute.
    DATA lo_isxml_element   TYPE REF TO lcl_isxml_element.

    CASE type.
      WHEN zif_excel_xml_node=>co_node_attribute.
        lo_isxml_attribute ?= me.
*        rval = lo_isxml_attribute->nsuri.
      WHEN zif_excel_xml_node=>co_node_element.
        lo_isxml_element ?= me.
        rval = lo_isxml_element->get_namespace_uri_by_prefix( lo_isxml_element->prefix ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_next.
    rval = next_sibling.
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_value.
    DATA lo_isxml_text          TYPE REF TO lcl_isxml_text.
    DATA lo_isxml_attribute     TYPE REF TO lcl_isxml_attribute.
    DATA lt_node                TYPE TABLE OF REF TO zif_excel_xml_node.
    DATA lo_node                TYPE REF TO zif_excel_xml_node.
    DATA lv_tabix               TYPE i.
    DATA lo_child_node_list     TYPE REF TO zif_excel_xml_node_list.
    DATA lo_child_node_iterator TYPE REF TO zif_excel_xml_node_iterator.
    DATA lo_child_node          TYPE REF TO zif_excel_xml_node.
    DATA lo_isxml_node          TYPE REF TO lcl_isxml_node.
    DATA lo_text                TYPE REF TO lcl_isxml_text.

    CASE type.
      WHEN zif_excel_xml_node=>co_node_text.
        lo_isxml_text ?= me.
        rval = lo_isxml_text->value.

      WHEN zif_excel_xml_node=>co_node_attribute.
        lo_isxml_attribute ?= me.
        rval = lo_isxml_attribute->value.

      WHEN zif_excel_xml_node=>co_node_document.
        rval = ''.

      WHEN zif_excel_xml_node=>co_node_element.

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
          lo_isxml_node ?= lo_node.
          CASE lo_isxml_node->type.
            WHEN zif_excel_xml_node=>co_node_text.
              lo_text ?= lo_node.
              rval = rval && lo_text->value.
          ENDCASE.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE lcx_unexpected.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_excel_xml_node~set_value.
    DATA lo_isxml_attribute TYPE REF TO lcl_isxml_attribute.
    DATA lo_isxml_element   TYPE REF TO zif_excel_xml_element.
    DATA lo_isxml_text      TYPE REF TO lcl_isxml_text.

    CASE type.
      WHEN zif_excel_xml_node=>co_node_attribute.
        lo_isxml_attribute ?= me.
        lo_isxml_attribute->value = value.
      WHEN zif_excel_xml_node=>co_node_element.
        lo_isxml_element ?= me.
        CREATE OBJECT lo_isxml_text.
        lo_isxml_text->type  = zif_excel_xml_node=>co_node_text.
        lo_isxml_text->value = value.
        lo_isxml_element->append_child( new_child = lo_isxml_text ).
      WHEN zif_excel_xml_node=>co_node_text.
        lo_isxml_text ?= me.
        lo_isxml_text->value = value.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node_collection IMPLEMENTATION.
  METHOD zif_excel_xml_node_collection~create_iterator.
    DATA lo_isxml_node_iterator TYPE REF TO lcl_isxml_node_iterator.

    CREATE OBJECT lo_isxml_node_iterator.
    lo_isxml_node_iterator->node_collection = me.

    rval = lo_isxml_node_iterator.
  ENDMETHOD.

  METHOD zif_excel_xml_node_collection~get_length.
    rval = lines( table_nodes ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node_iterator IMPLEMENTATION.
  METHOD zif_excel_xml_node_iterator~get_next.
    DATA lr_attribute TYPE REF TO lcl_isxml_element=>ts_attribute.
    DATA lo_next_node TYPE REF TO lcl_isxml_node.

    IF named_node_map IS BOUND.
      IF position < lines( named_node_map->element->attributes ).
        position = position + 1.
        READ TABLE named_node_map->element->attributes INDEX position REFERENCE INTO lr_attribute.
        IF sy-subrc = 0.
          rval = lr_attribute->object.
        ENDIF.
      ENDIF.

    ELSEIF node IS BOUND.
      IF position >= 0.
        position = position + 1.
        IF position = 1.
          current_node = node.
        ELSEIF position = 2 AND current_node->first_child IS NOT BOUND.
          CLEAR current_node.
          position = -1.
          EXIT.
        ELSEIF current_node->first_child IS BOUND.
          current_node = current_node->first_child.
        ELSE.
          lo_next_node = current_node.
          DO.
            IF lo_next_node->next_sibling IS BOUND.
              current_node = lo_next_node->next_sibling.
              EXIT.
            ENDIF.
            lo_next_node ?= lo_next_node->parent.
            IF lo_next_node = node.
              " Back to the starting node = end of iteration.
              CLEAR current_node.
              position = -1.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.
        rval = current_node.
      ENDIF.
    ELSEIF node_list IS BOUND.
      IF position < lines( node_list->table_nodes ).
        position = position + 1.
        READ TABLE node_list->table_nodes INDEX position INTO rval.
      ENDIF.
    ELSEIF node_collection IS BOUND.
      IF position < lines( node_collection->table_nodes ).
        position = position + 1.
        READ TABLE node_collection->table_nodes INDEX position INTO rval.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_node_list IMPLEMENTATION.
  METHOD zif_excel_xml_node_list~create_iterator.
    DATA lo_isxml_node_iterator TYPE REF TO lcl_isxml_node_iterator.

    CREATE OBJECT lo_isxml_node_iterator.
    lo_isxml_node_iterator->node_list = me.

    rval = lo_isxml_node_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_ostream_string IMPLEMENTATION.
  METHOD create.
    rval = NEW lcl_isxml_ostream_string( ).
    rval->ref_string                    = string.
    rval->lif_isxml_ostream~type        = 'C'.
    rval->lif_isxml_ostream~sxml_writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_ostream_xstring IMPLEMENTATION.
  METHOD create.
    rval = NEW lcl_isxml_ostream_xstring( ).
    rval->ref_xstring                   = xstring.
    rval->lif_isxml_ostream~type        = 'X'.
    rval->lif_isxml_ostream~sxml_writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_parser IMPLEMENTATION.
  METHOD zif_excel_xml_parser~add_strip_space_element.
    add_strip_space_element = abap_true.
  ENDMETHOD.

  METHOD zif_excel_xml_parser~parse.
    TYPES:
      BEGIN OF ts_level,
        level      TYPE i,
        neg_level  TYPE i,
        nsprefix   TYPE string,
        isxml_node TYPE REF TO lcl_isxml_node,
        nsbindings TYPE if_sxml_named=>nsbindings,
      END OF ts_level.

    DATA lv_current_level            TYPE i.
    DATA ls_level                    TYPE ts_level.
    DATA lt_level                    TYPE STANDARD TABLE OF ts_level WITH DEFAULT KEY.
    DATA lo_sxml_reader              TYPE REF TO if_sxml_reader.
    DATA lo_sxml_node                TYPE REF TO if_sxml_node.
    DATA lo_sxml_parse_error         TYPE REF TO cx_sxml_parse_error.
    DATA lo_sxml_node_close          TYPE REF TO if_sxml_close_element.
    DATA lo_isxml_element            TYPE REF TO lcl_isxml_element.
    DATA lo_isxml_text               TYPE REF TO lcl_isxml_text.
    DATA lv_value                    TYPE string.
    DATA lo_sxml_node_open           TYPE REF TO if_sxml_open_element.
    DATA lo_previous_isxml_attribute TYPE REF TO lcl_isxml_attribute.
    DATA lt_nsbinding                TYPE if_sxml_named=>nsbindings.
    DATA lr_nsbinding                TYPE REF TO if_sxml_named=>nsbinding.
    DATA lv_add_xmlns_attribute      TYPE abap_bool.
    DATA lr_nsbinding_2              TYPE REF TO if_sxml_named=>nsbinding.
    DATA ls_isxml_attribute          TYPE lcl_isxml_element=>ts_attribute.
    DATA lt_sxml_attribute           TYPE if_sxml_attribute=>attributes.
    DATA lo_sxml_attribute           TYPE REF TO if_sxml_attribute.
    DATA lo_sxml_node_value          TYPE REF TO if_sxml_value_node.

    FIELD-SYMBOLS <ls_level> TYPE ts_level.

    lv_current_level = 1.
    ls_level-level      = lv_current_level.
    ls_level-isxml_node = document.
    INSERT ls_level INTO TABLE lt_level ASSIGNING <ls_level>.

    lo_sxml_reader = istream->sxml_reader.

    DO.
      TRY.
          lo_sxml_node = lo_sxml_reader->read_next_node( ).
        CATCH cx_sxml_parse_error INTO lo_sxml_parse_error.
          RAISE EXCEPTION TYPE lcx_unexpected.
      ENDTRY.
      IF lo_sxml_node IS NOT BOUND.
        EXIT.
      ENDIF.

      CASE lo_sxml_node->type.
        WHEN lo_sxml_node->co_nt_attribute.
          "should not happen in OO parsing?
          RAISE EXCEPTION TYPE lcx_unexpected.

        WHEN lo_sxml_node->co_nt_element_close.
          lo_sxml_node_close ?= lo_sxml_node.

          IF    add_strip_space_element = abap_true
             OR normalizing             = abap_true.
            lo_isxml_element ?= <ls_level>-isxml_node.
            IF     lo_isxml_element->first_child       IS BOUND
               AND lo_isxml_element->first_child        = lo_isxml_element->first_child
               AND lo_isxml_element->first_child->type  = zif_excel_xml_node=>co_node_text.
              lo_isxml_text ?= lo_isxml_element->first_child.
              lv_value = lo_isxml_text->value.
              SHIFT lv_value RIGHT DELETING TRAILING space.
              SHIFT lv_value LEFT DELETING LEADING space.
              IF     add_strip_space_element = abap_false
                 AND normalizing             = abap_true.
                IF lv_value IS NOT INITIAL.
                  lo_isxml_text->value = lv_value.
                ENDIF.
              ELSEIF lv_value IS INITIAL.
                CLEAR lo_isxml_element->first_child.
                CLEAR lo_isxml_element->last_child.
              ENDIF.
            ENDIF.
          ENDIF.

          DELETE lt_level INDEX lv_current_level.
          lv_current_level = lv_current_level - 1.
          READ TABLE lt_level INDEX lv_current_level ASSIGNING <ls_level>.

        WHEN lo_sxml_node->co_nt_element_open.
          lo_sxml_node_open ?= lo_sxml_node.

          CREATE OBJECT lo_isxml_element.
          lo_isxml_element->type      = zif_excel_xml_node=>co_node_element.
          " case  input                 name  namespace  prefix
          " 1     <A xmlns="nsuri">     A     nsuri      (empty)
          " 2     <A xmlns="nsuri"><B>  B     nsuri      (empty)
          lo_isxml_element->name      = lo_sxml_node_open->qname-name.
          lo_isxml_element->namespace = lo_sxml_node_open->qname-namespace.
          lo_isxml_element->prefix    = lo_sxml_node_open->prefix.

          <ls_level>-isxml_node->zif_excel_xml_node~append_child( lo_isxml_element ).

          FREE lo_previous_isxml_attribute.

          lt_nsbinding = lo_sxml_reader->get_nsbindings( ).
          LOOP AT lt_nsbinding REFERENCE INTO lr_nsbinding.
            lv_add_xmlns_attribute = abap_false.
            READ TABLE <ls_level>-nsbindings WITH TABLE KEY prefix = lr_nsbinding->prefix
                 REFERENCE INTO lr_nsbinding_2.
            IF sy-subrc <> 0.
              INSERT lr_nsbinding->* INTO TABLE <ls_level>-nsbindings.
              lv_add_xmlns_attribute = abap_true.
            ELSEIF lr_nsbinding->nsuri <> lr_nsbinding_2->nsuri.
              lv_add_xmlns_attribute = abap_true.
            ENDIF.
            IF lv_add_xmlns_attribute = abap_true.
              IF lr_nsbinding->prefix IS INITIAL.
                " Default namespace
                ls_isxml_attribute-name   = 'xmlns'.
                ls_isxml_attribute-prefix = ''.
              ELSE.
                ls_isxml_attribute-name   = lr_nsbinding->prefix.
                ls_isxml_attribute-prefix = 'xmlns'.
              ENDIF.
              lo_isxml_element->append_attribute( iv_prefix = ls_isxml_attribute-prefix
                                                  iv_name   = ls_isxml_attribute-name
                                                  iv_value  = lr_nsbinding->nsuri ).
            ENDIF.
          ENDLOOP.

          lv_current_level = lv_current_level + 1.
          ls_level-level      = lv_current_level.
          ls_level-isxml_node = lo_isxml_element.
          ls_level-nsbindings = lt_nsbinding.
          INSERT ls_level INTO TABLE lt_level ASSIGNING <ls_level>.

          lt_sxml_attribute = lo_sxml_node_open->get_attributes( ).
          LOOP AT lt_sxml_attribute INTO lo_sxml_attribute.
            " SXML property values of XML attributes.
            " case  input                                         name  namespace  prefix
            " 1     <A xmlns:nsprefix="nsuri" nsprefix:attr="B">  attr  nsuri      nsprefix
            " 2     <A attr="B">                                  attr  (empty)    (empty)
            " 3     <nsprefix:A xmlns:nsprefix="nsuri" attr="B">  attr  (empty)    (empty)
            " 4     <A xmlns="dnsuri" attr="B">                   attr  (empty)    (empty)
            lo_isxml_element->append_attribute( iv_prefix = lo_sxml_attribute->prefix
                                                iv_name   = lo_sxml_attribute->qname-name
                                                iv_value  = lo_sxml_attribute->get_value( ) ).
          ENDLOOP.

        WHEN lo_sxml_node->co_nt_final.
          "should not happen?
          RAISE EXCEPTION TYPE lcx_unexpected.

        WHEN lo_sxml_node->co_nt_initial.
          "should not happen?
          RAISE EXCEPTION TYPE lcx_unexpected.

        WHEN lo_sxml_node->co_nt_value.
          lo_sxml_node_value ?= lo_sxml_node.
          CREATE OBJECT lo_isxml_text.
          lo_isxml_text->type  = zif_excel_xml_node=>co_node_text.
          lo_isxml_text->value = lo_sxml_node_value->get_value( ).

          <ls_level>-isxml_node->zif_excel_xml_node~append_child( lo_isxml_text ).

      ENDCASE.
    ENDDO.
  ENDMETHOD.

  METHOD zif_excel_xml_parser~set_normalizing.
    normalizing = is_normalizing.
    IF is_normalizing = abap_true.
      istream->sxml_reader->set_option( if_sxml_reader=>co_opt_normalizing ).
*    ELSE.
*      istream->sxml_reader->set_option( if_sxml_reader=>co_opt_keep_whitespace ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_xml_parser~set_validating.
    IF mode <> zif_excel_xml_parser=>co_no_validation.
      RAISE EXCEPTION TYPE zcx_excel_xml_not_implemented.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_renderer IMPLEMENTATION.
  METHOD zif_excel_xml_renderer~render.
    DATA lo_sxml_string_writer    TYPE REF TO cl_sxml_string_writer.
    DATA lv_xstring               TYPE xstring.
    DATA lo_isxml_ostream_string  TYPE REF TO lcl_isxml_ostream_string.
    DATA lv_xml_header_as_xstring TYPE xstring.
    DATA lo_isxml_ostream_xstring TYPE REF TO lcl_isxml_ostream_xstring.
    DATA lv_xml_body_as_xstring   TYPE xstring.

    " Debug helper: set trace_active = 'X'
    CLEAR elements_processed.
    CLEAR elements_traced.
    document->first_child->render( io_sxml_writer    = ostream->sxml_writer
                                   io_isxml_renderer = me ).

    CASE ostream->type.
      WHEN 'C'.
        lo_sxml_string_writer ?= ostream->sxml_writer.
        lv_xstring = lo_sxml_string_writer->get_output( ).
        lo_isxml_ostream_string ?= ostream.
        lo_isxml_ostream_string->ref_string->* = document->get_xml_header_as_string( ) && cl_abap_codepage=>convert_from(
                                                                                              lv_xstring ).
      WHEN 'X'.
        lv_xml_header_as_xstring = document->get_xml_header_as_xstring( ).
        lo_sxml_string_writer ?= ostream->sxml_writer.
        lo_isxml_ostream_xstring ?= ostream.
        lv_xml_body_as_xstring = lo_sxml_string_writer->get_output( ).
        CONCATENATE lv_xml_header_as_xstring
                    lv_xml_body_as_xstring
                    INTO lo_isxml_ostream_xstring->ref_xstring->*
                    IN BYTE MODE.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_stream IMPLEMENTATION.
  METHOD zif_excel_xml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_stream_factory IMPLEMENTATION.
  METHOD zif_excel_xml_stream_factory~create_istream_string.
    rval = lcl_isxml_istream_string=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_xml_stream_factory~create_istream_xstring.
    rval = lcl_isxml_istream_xstring=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_xml_stream_factory~create_ostream_cstring.
    rval = lcl_isxml_ostream_string=>create( string ).
  ENDMETHOD.

  METHOD zif_excel_xml_stream_factory~create_ostream_xstring.
    rval = lcl_isxml_ostream_xstring=>create( string ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_text IMPLEMENTATION.
  METHOD render.
    DATA lo_value_node TYPE REF TO if_sxml_value_node.

    lo_value_node = io_sxml_writer->new_value( ).
    lo_value_node->set_value( value ).
    io_sxml_writer->write_node( lo_value_node ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_isxml_unknown IMPLEMENTATION.
  METHOD split_name_into_qname.
    DATA lv_colon_position TYPE i.

    IF iv_prefix IS NOT INITIAL.
      rs_result-name   = iv_name.
      rs_result-prefix = iv_prefix.
    ELSE.
      lv_colon_position = find( val = iv_name
                                sub = ':' ).
      IF lv_colon_position >= 0.
        rs_result-prefix = substring( val = iv_name
                                      off = 0
                                      len = lv_colon_position ).
        rs_result-name   = substring( val = iv_name
                                      off = lv_colon_position + 1 ).
      ELSE.
        rs_result-prefix = ''.
        rs_result-name   = iv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_xml_unknown~query_interface.
    CASE iid.
      WHEN lcl_isxml=>ixml_iid-element.
        IF type = zif_excel_xml_node=>co_node_element.
          rval = me.
        ENDIF.
      WHEN lcl_isxml=>ixml_iid-text.
        IF type = zif_excel_xml_node=>co_node_text.
          rval = me.
        ENDIF.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE lcx_unexpected.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_rewrite_xml_via_sxml IMPLEMENTATION.
  METHOD execute.
    TYPES:
      BEGIN OF ts_level,
        number     TYPE i,
        nsbindings TYPE if_sxml_named=>nsbindings,
      END OF ts_level.

    DATA lv_current_level    TYPE i.
    DATA ls_level            TYPE ts_level.
    DATA lt_level            TYPE STANDARD TABLE OF ts_level WITH DEFAULT KEY.
    DATA lo_reader           TYPE REF TO if_sxml_reader.
    DATA lo_string_writer    TYPE REF TO cl_sxml_string_writer.
    DATA lo_writer           TYPE REF TO if_sxml_writer.
    DATA lo_node             TYPE REF TO if_sxml_node.
    DATA lo_close_element    TYPE REF TO if_sxml_close_element.
    DATA lo_open_element     TYPE REF TO if_sxml_open_element.
    DATA lt_nsbinding        TYPE if_sxml_named=>nsbindings.
    DATA lt_attribute        TYPE if_sxml_attribute=>attributes.
    DATA ls_complete_element TYPE ts_complete_element.
    DATA lr_nsbinding        TYPE REF TO if_sxml_named=>nsbinding.
    DATA ls_nsbinding        TYPE ts_nsbinding.
    DATA lo_attribute        TYPE REF TO if_sxml_attribute.
    DATA ls_attribute        TYPE ts_attribute.
    DATA lt_new_nsbinding    TYPE if_sxml_named=>nsbindings.
    DATA lo_value_node       TYPE REF TO if_sxml_value_node.
    DATA lv_string           TYPE string.

    FIELD-SYMBOLS <ls_level> TYPE ts_level.

    CLEAR complete_parsed_elements.

    lv_current_level = 1.
    ls_level-number = lv_current_level.
    INSERT ls_level INTO TABLE lt_level ASSIGNING <ls_level>.

    lo_reader = cl_sxml_string_reader=>create( input = cl_abap_codepage=>convert_to( iv_xml_string ) ).
    lo_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_xml10 ).
    lo_writer = lo_string_writer.

    DO.
      lo_node = lo_reader->read_next_node( ).
      IF lo_node IS NOT BOUND.
        " End of XML
        EXIT.
      ENDIF.

      CASE lo_node->type.
        WHEN lo_node->co_nt_attribute.
          "should not happen in OO parsing (READ_NEXT_NODE)
          RAISE EXCEPTION TYPE lcx_unexpected.

        WHEN lo_node->co_nt_element_close.

          DELETE lt_level INDEX lv_current_level.
          lv_current_level = lv_current_level - 1.
          READ TABLE lt_level INDEX lv_current_level ASSIGNING <ls_level>.

          lo_close_element = lo_writer->new_close_element( ).
          lo_writer->write_node( lo_close_element ).

        WHEN lo_node->co_nt_element_open.
          lo_open_element ?= lo_node.

          lt_nsbinding = lo_reader->get_nsbindings( ).
          lt_attribute = lo_open_element->get_attributes( ).

          IF iv_trace = abap_true.
            CLEAR ls_complete_element.
            ls_complete_element-element-name      = lo_open_element->qname-name.
            ls_complete_element-element-namespace = lo_open_element->qname-namespace.
            ls_complete_element-element-prefix    = lo_open_element->prefix.
            LOOP AT lt_nsbinding REFERENCE INTO lr_nsbinding.
              ls_nsbinding-prefix = lr_nsbinding->prefix.
              ls_nsbinding-nsuri  = lr_nsbinding->nsuri.
              INSERT ls_nsbinding INTO TABLE ls_complete_element-nsbindings.
            ENDLOOP.
            LOOP AT lt_attribute INTO lo_attribute.
              ls_attribute-name      = lo_attribute->qname-name.
              ls_attribute-namespace = lo_attribute->qname-namespace.
              ls_attribute-prefix    = lo_attribute->prefix.
              INSERT ls_attribute INTO TABLE ls_complete_element-attributes.
            ENDLOOP.
            INSERT ls_complete_element INTO TABLE complete_parsed_elements.
          ENDIF.

          IF lo_open_element->prefix IS INITIAL.
            lo_open_element = lo_writer->new_open_element( name = lo_open_element->qname-name ).
          ELSE.
            lo_open_element = lo_writer->new_open_element( name   = lo_open_element->qname-name
                                                           nsuri  = lo_open_element->qname-namespace
                                                           prefix = lo_open_element->prefix ).
          ENDIF.

          lo_open_element->set_attributes( lt_attribute ).

          CLEAR lt_new_nsbinding.
          LOOP AT lt_nsbinding REFERENCE INTO lr_nsbinding.
            READ TABLE <ls_level>-nsbindings TRANSPORTING NO FIELDS WITH KEY prefix = lr_nsbinding->prefix
                                                                             nsuri  = lr_nsbinding->nsuri.
            IF sy-subrc <> 0.
              " It's the first time the default namespace is used,
              " or if it has been changed, then declare it.
              " (the default namespace must be set via set_attribute before the element
              " is written, while other namespaces must be written using
              " write_namespace_declaration after the element is written)
              IF lr_nsbinding->prefix IS INITIAL.
                lo_open_element->set_attribute( name  = 'xmlns'
                                                value = lr_nsbinding->nsuri ).
              ELSE.
                INSERT lr_nsbinding->* INTO TABLE lt_new_nsbinding.
              ENDIF.
            ENDIF.
          ENDLOOP.

          lv_current_level = lv_current_level + 1.
          ls_level-number     = lv_current_level.
          ls_level-nsbindings = lt_nsbinding.
          INSERT ls_level INTO TABLE lt_level ASSIGNING <ls_level>.

          lo_writer->write_node( node = lo_open_element ).

          LOOP AT lt_new_nsbinding REFERENCE INTO lr_nsbinding
               WHERE prefix IS NOT INITIAL.
            lo_writer->write_namespace_declaration( nsuri  = lr_nsbinding->nsuri
                                                    prefix = lr_nsbinding->prefix ).
          ENDLOOP.

        WHEN lo_node->co_nt_final.
          "should not happen in OO parsing
          RAISE EXCEPTION TYPE lcx_unexpected.

        WHEN lo_node->co_nt_initial.
          "should not happen in OO parsing
          RAISE EXCEPTION TYPE lcx_unexpected.

        WHEN lo_node->co_nt_value.

          lo_value_node ?= lo_node.
          lv_string = lo_value_node->get_value( ).

          lo_value_node = lo_writer->new_value( ).
          lo_value_node->set_value( lv_string ).
          lo_writer->write_node( lo_value_node ).

        WHEN OTHERS.
          "should not happen whatever it's OO or token parsing
          RAISE EXCEPTION TYPE lcx_unexpected.
      ENDCASE.
    ENDDO.
    rv_string = cl_abap_codepage=>convert_from( lo_string_writer->get_output( ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml IMPLEMENTATION.
  METHOD create.
    IF singleton IS NOT BOUND.
      CREATE OBJECT singleton.
      singleton->ixml = cl_ixml=>create( ).
    ENDIF.
    ro_result = singleton.
  ENDMETHOD.

  METHOD unwrap_ixml.
    DATA lr_wrapped_ixml_object TYPE REF TO ts_wrapped_ixml_object.

    CHECK io_wrap_ixml_unknown IS BOUND.

    READ TABLE wrapped_ixml_objects
         WITH TABLE KEY by_wrapper
         COMPONENTS ixml_object_wrapper = io_wrap_ixml_unknown
         REFERENCE INTO lr_wrapped_ixml_object.
    IF sy-subrc = 0.
      ro_result = lr_wrapped_ixml_object->ixml_object.
    ENDIF.
  ENDMETHOD.

  METHOD wrap_ixml.
    DATA lr_wrapped_ixml_object       TYPE REF TO ts_wrapped_ixml_object.
    DATA ls_wrapped_ixml_object       TYPE ts_wrapped_ixml_object.
    DATA lv_class_name                TYPE string.
    DATA lo_wrap_ixml_attribute       TYPE REF TO lth_wrap_ixml_attribute.
    DATA lo_wrap_ixml_node            TYPE REF TO lth_wrap_ixml_node.
    DATA lo_wrap_ixml_document        TYPE REF TO lth_wrap_ixml_document.
    DATA lo_wrap_ixml_element         TYPE REF TO lth_wrap_ixml_element.
    DATA lo_wrap_ixml_named_node_map  TYPE REF TO lth_wrap_ixml_named_node_map.
    DATA lo_wrap_ixml_node_collection TYPE REF TO lth_wrap_ixml_node_collection.
    DATA lo_wrap_ixml_node_iterator   TYPE REF TO lth_wrap_ixml_node_iterator.
    DATA lo_wrap_ixml_node_list       TYPE REF TO lth_wrap_ixml_node_list.
    DATA lo_wrap_ixml_text            TYPE REF TO lth_wrap_ixml_text.

    IF io_ixml_unknown IS NOT BOUND.
      RETURN.
    ENDIF.

    READ TABLE wrapped_ixml_objects WITH TABLE KEY ixml_object = io_ixml_unknown
         REFERENCE INTO lr_wrapped_ixml_object.
    IF sy-subrc <> 0.
      CLEAR ls_wrapped_ixml_object.
      ls_wrapped_ixml_object-ixml_object = io_ixml_unknown.
      lv_class_name = cl_abap_typedescr=>describe_by_object_ref( io_ixml_unknown )->get_relative_name( ).
      CASE lv_class_name.
        WHEN 'CL_IXML_ATTRIBUTE'.
          CREATE OBJECT lo_wrap_ixml_attribute.
          lo_wrap_ixml_attribute->ixml_attribute ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_attribute.
          lo_wrap_ixml_node ?= lo_wrap_ixml_attribute.
          lo_wrap_ixml_node->ixml_node ?= io_ixml_unknown.
        WHEN 'CL_IXML_DOCUMENT'.
          CREATE OBJECT lo_wrap_ixml_document.
          lo_wrap_ixml_document->ixml_document ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_document.
          lo_wrap_ixml_node ?= lo_wrap_ixml_document.
          lo_wrap_ixml_node->ixml_node ?= io_ixml_unknown.
        WHEN 'CL_IXML_ELEMENT'.
          CREATE OBJECT lo_wrap_ixml_element.
          lo_wrap_ixml_element->ixml_element ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_element.
          lo_wrap_ixml_node ?= lo_wrap_ixml_element.
          lo_wrap_ixml_node->ixml_node ?= io_ixml_unknown.
        WHEN 'CL_IXML_NAMED_NODE_MAP'.
          CREATE OBJECT lo_wrap_ixml_named_node_map.
          lo_wrap_ixml_named_node_map->ixml_named_node_map ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_named_node_map.
        WHEN 'CL_IXML_NODE_COLLECTION'.
          CREATE OBJECT lo_wrap_ixml_node_collection.
          lo_wrap_ixml_node_collection->ixml_node_collection ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_node_collection.
        WHEN 'CL_IXML_NODE_ITERATOR'.
          CREATE OBJECT lo_wrap_ixml_node_iterator.
          lo_wrap_ixml_node_iterator->ixml_node_iterator ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_node_iterator.
        WHEN 'CL_IXML_NODE_LIST'.
          CREATE OBJECT lo_wrap_ixml_node_list.
          lo_wrap_ixml_node_list->ixml_node_list ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_node_list.
        WHEN 'CL_IXML_TEXT'.
          CREATE OBJECT lo_wrap_ixml_text.
          lo_wrap_ixml_text->ixml_text ?= io_ixml_unknown.
          ls_wrapped_ixml_object-ixml_object_wrapper = lo_wrap_ixml_text.
          lo_wrap_ixml_node ?= lo_wrap_ixml_text.
          lo_wrap_ixml_node->ixml_node ?= io_ixml_unknown.
      ENDCASE.
      INSERT ls_wrapped_ixml_object INTO TABLE wrapped_ixml_objects REFERENCE INTO lr_wrapped_ixml_object.
    ENDIF.

    ro_result = lr_wrapped_ixml_object->ixml_object_wrapper.
  ENDMETHOD.

  METHOD zif_excel_xml~create_document.
    DATA lo_ixml_document TYPE REF TO if_ixml_document.

    lo_ixml_document = ixml->create_document( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_document ).
  ENDMETHOD.

  METHOD zif_excel_xml~create_encoding.
    DATA lo_encoding TYPE REF TO lth_wrap_ixml_encoding.

    CREATE OBJECT lo_encoding.
    lo_encoding->ixml_encoding = ixml->create_encoding( byte_order    = byte_order
                                                        character_set = character_set ).
    rval = lo_encoding.
  ENDMETHOD.

  METHOD zif_excel_xml~create_parser.
    DATA parser                   TYPE REF TO lth_wrap_ixml_parser.
    DATA wrap_ixml_document       TYPE REF TO lth_wrap_ixml_document.
    DATA wrap_ixml_istream        TYPE REF TO lif_wrap_ixml_istream.
    DATA wrap_ixml_stream_factory TYPE REF TO lth_wrap_ixml_stream_factory.

    CREATE OBJECT parser.
    wrap_ixml_document ?= document.
    wrap_ixml_istream ?= istream.
    wrap_ixml_stream_factory ?= stream_factory.
    parser->ixml_parser = ixml->create_parser( document       = wrap_ixml_document->ixml_document
                                               istream        = wrap_ixml_istream->ixml_istream
                                               stream_factory = wrap_ixml_stream_factory->ixml_stream_factory ).
    rval = parser.
  ENDMETHOD.

  METHOD zif_excel_xml~create_renderer.
    DATA lo_renderer        TYPE REF TO lth_wrap_ixml_renderer.
    DATA wrap_ixml_document TYPE REF TO lth_wrap_ixml_document.
    DATA wrap_ixml_ostream  TYPE REF TO lif_wrap_ixml_ostream.

    CREATE OBJECT lo_renderer.
    wrap_ixml_document ?= document.
    wrap_ixml_ostream ?= ostream.
    lo_renderer->ixml_renderer ?= ixml->create_renderer( document = wrap_ixml_document->ixml_document
                                                         ostream  = wrap_ixml_ostream->ixml_ostream ).
    rval = lo_renderer.
  ENDMETHOD.

  METHOD zif_excel_xml~create_stream_factory.
    DATA lo_stream_factory TYPE REF TO lth_wrap_ixml_stream_factory.

    CREATE OBJECT lo_stream_factory.
    lo_stream_factory->ixml_stream_factory = ixml->create_stream_factory( ).
    rval = lo_stream_factory.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_attribute IMPLEMENTATION.

ENDCLASS.


CLASS lth_wrap_ixml_character_data IMPLEMENTATION.

ENDCLASS.


CLASS lth_wrap_ixml_document IMPLEMENTATION.
  METHOD zif_excel_xml_document~create_element.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->create_element( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~create_simple_element.
    DATA lo_wrap_ixml_parent TYPE REF TO lth_wrap_ixml_node.
    DATA lo_ixml_element     TYPE REF TO if_ixml_element.

    lo_wrap_ixml_parent ?= parent.
    lo_ixml_element = ixml_document->create_simple_element( name   = name
                                                            parent = lo_wrap_ixml_parent->ixml_node ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~create_simple_element_ns.
    DATA lo_wrap_ixml_parent TYPE REF TO lth_wrap_ixml_node.
    DATA lo_ixml_element     TYPE REF TO if_ixml_element.

    lo_wrap_ixml_parent ?= parent.
    lo_ixml_element = ixml_document->create_simple_element_ns( name   = name
                                                               parent = lo_wrap_ixml_parent->ixml_node
                                                               prefix = prefix ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~find_from_name.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->find_from_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~find_from_name_ns.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->find_from_name_ns( name = name
                                                        uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~get_elements_by_tag_name.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_document->get_elements_by_tag_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~get_elements_by_tag_name_ns.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_document->get_elements_by_tag_name_ns( name = name
                                                                          uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~get_root_element.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->get_root_element( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~set_declaration.
    ixml_document->set_declaration( declaration = declaration ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~set_encoding.
    DATA wrap_ixml_encoding TYPE REF TO lth_wrap_ixml_encoding.

    wrap_ixml_encoding ?= encoding.
    ixml_document->set_encoding( encoding = wrap_ixml_encoding->ixml_encoding ).
  ENDMETHOD.

  METHOD zif_excel_xml_document~set_standalone.
    ixml_document->set_standalone( standalone ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_element IMPLEMENTATION.
  METHOD zif_excel_xml_element~find_from_name.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_element->find_from_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~find_from_name_ns.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_element->find_from_name_ns( depth = depth
                                                       name  = name
                                                       uri   = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_attribute.
    rval = ixml_element->get_attribute( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_attribute_node_ns.
    DATA lo_ixml_attribute TYPE REF TO if_ixml_attribute.

    lo_ixml_attribute = ixml_element->get_attribute_node_ns( name = name
                                                             uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_attribute ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_attribute_ns.
    rval = ixml_element->get_attribute_ns( name = name
                                           uri  = uri ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_elements_by_tag_name.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_element->get_elements_by_tag_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~get_elements_by_tag_name_ns.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_element->get_elements_by_tag_name_ns( name = name
                                                                         uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~remove_attribute_ns.
    ixml_element->remove_attribute_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~set_attribute.
    ixml_element->set_attribute( name      = name
                                 namespace = namespace
                                 value     = value ).
  ENDMETHOD.

  METHOD zif_excel_xml_element~set_attribute_ns.
    ixml_element->set_attribute_ns( name   = name
                                    prefix = prefix
                                    value  = value ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_istream_string IMPLEMENTATION.
  METHOD zif_excel_xml_stream~close.
    lif_wrap_ixml_istream~ixml_istream->close( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_istream_xstring IMPLEMENTATION.
  METHOD zif_excel_xml_stream~close.
    lif_wrap_ixml_istream~ixml_istream->close( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_named_node_map IMPLEMENTATION.
  METHOD zif_excel_xml_named_node_map~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_named_node_map->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node IMPLEMENTATION.
  METHOD zif_excel_xml_node~append_child.
    DATA lo_wrap_ixml_node TYPE REF TO lth_wrap_ixml_node.
    DATA lo_ixml_node      TYPE REF TO if_ixml_node.

    lo_wrap_ixml_node ?= new_child.
    lo_ixml_node ?= lth_wrap_ixml=>unwrap_ixml( lo_wrap_ixml_node ).
    ixml_node->append_child( lo_ixml_node ).
*    ixml_node->append_child( lo_wrap_ixml_node->ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~clone.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node->clone( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_node->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_attributes.
    DATA lo_ixml_named_node_map TYPE REF TO if_ixml_named_node_map.

    lo_ixml_named_node_map = ixml_node->get_attributes( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_named_node_map ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_children.
    DATA lo_ixml_node_list TYPE REF TO if_ixml_node_list.

    lo_ixml_node_list = ixml_node->get_children( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_list ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_first_child.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node->get_first_child( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_name.
    rval = ixml_node->get_name( ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_namespace_prefix.
    rval = ixml_node->get_namespace_prefix( ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_namespace_uri.
    rval = ixml_node->get_namespace_uri( ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_next.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node->get_next( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~get_value.
    rval = ixml_node->get_value( ).
  ENDMETHOD.

  METHOD zif_excel_xml_node~set_value.
    ixml_node->set_value( value ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node_collection IMPLEMENTATION.
  METHOD zif_excel_xml_node_collection~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_node_collection->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.

  METHOD zif_excel_xml_node_collection~get_length.
    rval = ixml_node_collection->get_length( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node_iterator IMPLEMENTATION.
  METHOD zif_excel_xml_node_iterator~get_next.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node_iterator->get_next( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node_list IMPLEMENTATION.
  METHOD zif_excel_xml_node_list~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_node_list->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_ostream_string IMPLEMENTATION.

ENDCLASS.


CLASS lth_wrap_ixml_parser IMPLEMENTATION.
  METHOD zif_excel_xml_parser~add_strip_space_element.
* Method PARSE_STRING of class ZCL_EXCEL_THEME_FMT_SCHEME
*    li_parser->add_strip_space_element( ).
    ixml_parser->add_strip_space_element( ).
  ENDMETHOD.

  METHOD zif_excel_xml_parser~parse.
    ixml_parser->parse( ).
  ENDMETHOD.

  METHOD zif_excel_xml_parser~set_normalizing.
    ixml_parser->set_normalizing( is_normalizing ).
  ENDMETHOD.

  METHOD zif_excel_xml_parser~set_validating.
    ixml_parser->set_validating( mode ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_renderer IMPLEMENTATION.
  METHOD zif_excel_xml_renderer~render.
    ixml_renderer->render( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_stream IMPLEMENTATION.
  METHOD zif_excel_xml_stream~close.
    RAISE EXCEPTION TYPE lcx_unexpected.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_stream_factory IMPLEMENTATION.
  METHOD zif_excel_xml_stream_factory~create_istream_string.
* Method PARSE_STRING of class ZCL_EXCEL_THEME_FMT_SCHEME:
*    li_ixml = cl_ixml=>create( ).
*    li_document = li_ixml->create_document( ).
*    li_factory = li_ixml->create_stream_factory( ).
*    li_istream = li_factory->create_istream_string( iv_string ).

    DATA lo_wrap_ixml_istream_string TYPE REF TO lth_wrap_ixml_istream_string.

    CREATE OBJECT lo_wrap_ixml_istream_string.
    lo_wrap_ixml_istream_string->lif_wrap_ixml_istream~ixml_istream = ixml_stream_factory->create_istream_string( string ).
    rval = lo_wrap_ixml_istream_string.
  ENDMETHOD.

  METHOD zif_excel_xml_stream_factory~create_istream_xstring.
* Method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007
*     lo_istream        = lo_streamfactory->create_istream_xstring( lv_content ).

    DATA lo_wrap_ixml_istream_xstring TYPE REF TO lth_wrap_ixml_istream_xstring.

    CREATE OBJECT lo_wrap_ixml_istream_xstring.
    lo_wrap_ixml_istream_xstring->lif_wrap_ixml_istream~ixml_istream = ixml_stream_factory->create_istream_xstring(
                                                                           string ).
    rval = lo_wrap_ixml_istream_xstring.
  ENDMETHOD.

  METHOD zif_excel_xml_stream_factory~create_ostream_cstring.
* Method RENDER_XML_DOCUMENT of class ZCL_EXCEL_WRITER_2007:
*    lo_streamfactory = me->ixml->create_stream_factory( ).
*    lo_ostream = lo_streamfactory->create_ostream_cstring( string = lv_string ).
*    lo_renderer = me->ixml->create_renderer( ostream  = lo_ostream document = io_document ).
*    lo_renderer->render( ).

    DATA lo_wrap_ixml_ostream_string TYPE REF TO lth_wrap_ixml_ostream_string.

    CREATE OBJECT lo_wrap_ixml_ostream_string.
    lo_wrap_ixml_ostream_string->lif_wrap_ixml_ostream~ixml_ostream = ixml_stream_factory->create_ostream_cstring(
                                                                          string->* ).
    rval = lo_wrap_ixml_ostream_string.
  ENDMETHOD.

  METHOD zif_excel_xml_stream_factory~create_ostream_xstring.
* Method CREATE_CONTENT_TYPES of class ZCL_EXCEL_WRITER_XLSM:
*    CLEAR ep_content.
*    lo_ixml = cl_ixml=>create( ).
*    lo_streamfactory = lo_ixml->create_stream_factory( ).
*    lo_ostream = lo_streamfactory->create_ostream_xstring( string = ep_content ).
*    lo_renderer = lo_ixml->create_renderer( ostream  = lo_ostream document = lo_document ).
*    lo_renderer->render( ).

    DATA lo_wrap_ixml_ostream_xstring TYPE REF TO lth_wrap_ixml_ostream_xstring.

    CREATE OBJECT lo_wrap_ixml_ostream_xstring.
    lo_wrap_ixml_ostream_xstring->lif_wrap_ixml_ostream~ixml_ostream = ixml_stream_factory->create_ostream_xstring(
                                                                           string->* ).
    rval = lo_wrap_ixml_ostream_xstring.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_text IMPLEMENTATION.

ENDCLASS.


CLASS lth_wrap_ixml_unknown IMPLEMENTATION.
  METHOD zif_excel_xml_unknown~query_interface.
    RAISE EXCEPTION TYPE lcx_unexpected.
  ENDMETHOD.
ENDCLASS.
