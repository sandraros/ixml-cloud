*"* use this source file for your ABAP unit test classes

"! SXML is less tolerant than IXML
CLASS ltc_diff_ixml_sxml_parser DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "! Parsing of &lt;a:A/> is possible with IXML, not with SXML
    METHODS missing_namespace_declaration FOR TESTING RAISING cx_static_check.

    DATA open_element TYPE REF TO if_sxml_open_element.
    DATA node         TYPE REF TO if_sxml_node.
    DATA reader       TYPE REF TO if_sxml_reader.
    DATA string       TYPE string.
    DATA xstring      TYPE xstring.
    DATA parse_error  TYPE REF TO cx_sxml_parse_error.
    DATA error_rtti   TYPE REF TO cl_abap_typedescr.
    DATA node_open    TYPE REF TO if_sxml_open_element.
    DATA error        TYPE REF TO cx_root.
    DATA node_attr    TYPE REF TO if_sxml_value.
    DATA value_node   TYPE REF TO if_sxml_value_node.

ENDCLASS.


CLASS ltc_ixml DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PROTECTED SECTION.

    DATA rc             TYPE i.
    DATA num_errors     TYPE i.
    DATA reason         TYPE string.
    DATA error          TYPE REF TO if_ixml_parse_error.
    DATA xstring        TYPE xstring.
    DATA ixml           TYPE REF TO if_ixml.
    DATA stream_factory TYPE REF TO if_ixml_stream_factory.
    DATA istream        TYPE REF TO if_ixml_istream.
    DATA parser         TYPE REF TO if_ixml_parser.
    DATA element        TYPE REF TO if_ixml_element.
    DATA string         TYPE string.
    DATA document       TYPE REF TO if_ixml_document.

ENDCLASS.


CLASS ltc_ixml_element DEFINITION
      INHERITING FROM ltc_ixml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS get_attribute_xmlns FOR TESTING RAISING cx_static_check.
    METHODS set_attribute_ns_name_xmlns FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_ixml_node DEFINITION
      INHERITING FROM ltc_ixml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS append_child FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_ixml_isxml DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PROTECTED SECTION.

    TYPES tt_ixml_and_isxml TYPE STANDARD TABLE OF REF TO zif_excel_ixml WITH DEFAULT KEY.

    CLASS-DATA ixml  TYPE REF TO zif_excel_ixml.
    CLASS-DATA isxml TYPE REF TO zif_excel_ixml.
    DATA ixml_or_isxml  TYPE REF TO zif_excel_ixml.
    DATA ixml_and_isxml TYPE tt_ixml_and_isxml.
    DATA encoding       TYPE REF TO zif_excel_ixml_encoding.
    DATA document       TYPE REF TO zif_excel_ixml_document.
    DATA type       TYPE i.
    DATA value       TYPE string.
    DATA element        TYPE REF TO zif_excel_ixml_element.
    DATA node           TYPE REF TO zif_excel_ixml_node.
    DATA attribute      TYPE REF TO zif_excel_ixml_attribute.
    DATA text           TYPE REF TO zif_excel_ixml_text.
    DATA xstring        TYPE xstring.
    DATA ref_xstring    TYPE REF TO xstring.
    DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.
    DATA named_node_map TYPE REF TO zif_excel_ixml_named_node_map.
    DATA node_iterator  TYPE REF TO zif_excel_ixml_node_iterator.
    DATA node_list      TYPE REF TO zif_excel_ixml_node_list.
    DATA ostream        TYPE REF TO zif_excel_ixml_ostream.
    DATA renderer       TYPE REF TO zif_excel_ixml_renderer.
    DATA string         TYPE string.
    DATA rc             TYPE i.
    DATA istream        TYPE REF TO zif_excel_ixml_istream.
    DATA parser         TYPE REF TO zif_excel_ixml_parser.

    CLASS-METHODS get_ixml_and_isxml
      RETURNING
        VALUE(rt_result) TYPE tt_ixml_and_isxml.

    METHODS render
      RETURNING
        VALUE(rv_result) TYPE string.

  PRIVATE SECTION.

    METHODS create_encoding FOR TESTING RAISING cx_static_check.

    METHODS setup.

ENDCLASS.


"! Test of ZIF_EXCEL_IXML_DOCUMENT methods
CLASS ltc_ixml_isxml_document DEFINITION
      INHERITING FROM ltc_ixml_isxml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_element FOR TESTING RAISING cx_static_check.
    METHODS create_simple_element FOR TESTING RAISING cx_static_check.
    METHODS create_simple_element_ns FOR TESTING RAISING cx_static_check.
    METHODS find_from_name FOR TESTING RAISING cx_static_check.
    METHODS find_from_name_ns FOR TESTING RAISING cx_static_check.
    METHODS get_elements_by_tag_name FOR TESTING RAISING cx_static_check.
    METHODS get_root_element FOR TESTING RAISING cx_static_check.
    METHODS set_encoding FOR TESTING RAISING cx_static_check.
    METHODS set_standalone FOR TESTING RAISING cx_static_check.

    METHODS setup.

ENDCLASS.


"! Test of ZIF_EXCEL_IXML_ELEMENT methods
CLASS ltc_ixml_isxml_element DEFINITION
      INHERITING FROM ltc_ixml_isxml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS find_from_name_level_1      FOR TESTING RAISING cx_static_check.
    METHODS find_from_name_level_2      FOR TESTING RAISING cx_static_check.
    METHODS find_from_name_ns_depth_0   FOR TESTING RAISING cx_static_check.
    METHODS find_from_name_ns_depth_1   FOR TESTING RAISING cx_static_check.
    METHODS find_from_name_ns_depth_2   FOR TESTING RAISING cx_static_check.
    METHODS get_attribute               FOR TESTING RAISING cx_static_check.
    METHODS get_attribute_node_ns       FOR TESTING RAISING cx_static_check.
    METHODS get_attribute_ns            FOR TESTING RAISING cx_static_check.
    METHODS get_elements_by_tag_name    FOR TESTING RAISING cx_static_check.
    METHODS get_elements_by_tag_name_ns FOR TESTING RAISING cx_static_check.
    METHODS remove_attribute_ns         FOR TESTING RAISING cx_static_check.
    METHODS set_attribute               FOR TESTING RAISING cx_static_check.
    METHODS set_attribute_ns            FOR TESTING RAISING cx_static_check.

    METHODS find_from_name_ns
      IMPORTING
        iv_depth     TYPE i
        iv_exp_found TYPE abap_bool
        iv_exp_value TYPE string OPTIONAL.
    METHODS setup.

ENDCLASS.


"! Test of ZIF_EXCEL_IXML_NODE methods
CLASS ltc_ixml_isxml_node DEFINITION
      INHERITING FROM ltc_ixml_isxml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS append_child FOR TESTING RAISING cx_static_check.
    METHODS clone FOR TESTING RAISING cx_static_check.
    METHODS create_iterator FOR TESTING RAISING cx_static_check.
    METHODS get_attributes FOR TESTING RAISING cx_static_check.
    METHODS get_children FOR TESTING RAISING cx_static_check.
    METHODS get_first_child FOR TESTING RAISING cx_static_check.
    METHODS get_name FOR TESTING RAISING cx_static_check.
    METHODS get_namespace_prefix FOR TESTING RAISING cx_static_check.
    METHODS get_namespace_uri FOR TESTING RAISING cx_static_check.
    METHODS get_next FOR TESTING RAISING cx_static_check.
    METHODS get_type FOR TESTING RAISING cx_static_check.
    METHODS get_value FOR TESTING RAISING cx_static_check.
    METHODS set_namespace_prefix FOR TESTING RAISING cx_static_check.
    METHODS set_value FOR TESTING RAISING cx_static_check.


*    METHODS get_xml_document
*      IMPORTING
*        xml_string    TYPE string
*      RETURNING
*        VALUE(result) TYPE REF TO if_ixml_document.

    METHODS setup.
ENDCLASS.


CLASS ltc_ixml_isxml_parser DEFINITION
      INHERITING FROM ltc_ixml_isxml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS add_strip_space_element FOR TESTING RAISING cx_static_check.
    METHODS namespace FOR TESTING RAISING cx_static_check.
    METHODS set_normalizing FOR TESTING RAISING cx_static_check.
    METHODS set_validating FOR TESTING RAISING cx_static_check.
    METHODS several_children FOR TESTING RAISING cx_static_check.
    METHODS text_node FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_instances FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_stream_factories FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_encodings FOR TESTING RAISING cx_static_check.
*    METHODS two_parsers FOR TESTING RAISING cx_static_check.

    METHODS setup.

    METHODS parse
      IMPORTING
        iv_string TYPE string
      RETURNING
        VALUE(ro_document) TYPE REF TO zif_excel_ixml_document.
ENDCLASS.


CLASS ltc_ixml_isxml_render DEFINITION
      INHERITING FROM ltc_ixml_isxml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS most_simple_valid_xml FOR TESTING RAISING cx_static_check.
    METHODS namespace FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_ixml_parser DEFINITION
      INHERITING FROM ltc_ixml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS empty_xml FOR TESTING RAISING cx_static_check.
    METHODS end_tag_doesnt_match_begin_tag FOR TESTING RAISING cx_static_check.
    METHODS most_simple_valid_xml FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_rewrite_xml_via_sxml DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS default_namespace FOR TESTING RAISING cx_static_check.
    METHODS default_namespace_removed FOR TESTING RAISING cx_static_check.
    METHODS namespace FOR TESTING RAISING cx_static_check.
    METHODS namespace_2 FOR TESTING RAISING cx_static_check.
    METHODS namespace_3 FOR TESTING RAISING cx_static_check.

    DATA parsed_element_index TYPE i.
    DATA string               TYPE string.

    METHODS get_expected_attribute
      IMPORTING
        iv_name          TYPE string
        iv_namespace     TYPE string DEFAULT ``
        iv_prefix        TYPE string DEFAULT ``
      RETURNING
        VALUE(rs_result) TYPE lcl_rewrite_xml_via_sxml=>ts_attribute.

    METHODS get_expected_element
      IMPORTING
        iv_name          TYPE string
        iv_namespace     TYPE string DEFAULT ``
        iv_prefix        TYPE string DEFAULT ``
      RETURNING
        VALUE(rs_result) TYPE lcl_rewrite_xml_via_sxml=>ts_element.

    METHODS get_expected_nsbinding
      IMPORTING
        iv_prefix        TYPE string DEFAULT ``
        iv_nsuri         TYPE string DEFAULT ``
      RETURNING
        VALUE(rs_result) TYPE lcl_rewrite_xml_via_sxml=>ts_nsbinding.

    METHODS get_parsed_element
      RETURNING
        VALUE(rs_result) TYPE lcl_rewrite_xml_via_sxml=>ts_element.

    METHODS get_parsed_element_attribute
      IMPORTING
        iv_index         TYPE i
      RETURNING
        VALUE(rs_result) TYPE lcl_rewrite_xml_via_sxml=>ts_attribute.

    METHODS get_parsed_element_nsbinding
      IMPORTING
        iv_index         TYPE i
      RETURNING
        VALUE(rs_result) TYPE lcl_rewrite_xml_via_sxml=>ts_nsbinding.

    METHODS rewrite_xml_via_sxml
      IMPORTING
        iv_xml_string TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.

    METHODS set_current_parsed_element
      IMPORTING
        iv_index TYPE i.

ENDCLASS.


CLASS ltc_sxml_reader DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS empty_object_oriented_parsing FOR TESTING RAISING cx_static_check.
    METHODS empty_token_based_parsing FOR TESTING RAISING cx_static_check.
    METHODS empty_xml FOR TESTING RAISING cx_static_check.
    METHODS invalid_xml FOR TESTING RAISING cx_static_check.
    METHODS invalid_xml_eof_reached FOR TESTING RAISING cx_static_check.
    METHODS invalid_xml_not_wellformed FOR TESTING RAISING cx_static_check.
    METHODS object_oriented_parsing FOR TESTING RAISING cx_static_check.
    METHODS token_based_parsing FOR TESTING RAISING cx_static_check.
    METHODS xml_header_is_ignored FOR TESTING RAISING cx_static_check.

    DATA node        TYPE REF TO if_sxml_node.
    DATA reader      TYPE REF TO if_sxml_reader.
    DATA xstring     TYPE xstring.
    DATA parse_error TYPE REF TO cx_sxml_parse_error.
    DATA error_rtti  TYPE REF TO cl_abap_typedescr.
    DATA node_open   TYPE REF TO if_sxml_open_element.
    DATA error       TYPE REF TO cx_root.
    DATA node_attr   TYPE REF TO if_sxml_value.
    DATA value_node  TYPE REF TO if_sxml_value_node.
ENDCLASS.


CLASS ltc_sxml_writer DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS attribute_namespace FOR TESTING RAISING cx_static_check.
    "! It's mandatory to indicate nsuri = 'http://www.w3.org/XML/1998/namespace'
    "! for the standard "xml" namespace, but the URI is not rendered.
    "! e.g. open_element->set_attribute( name = 'space' nsuri = 'http://www.w3.org/XML/1998/namespace' prefix = 'xml' value = 'preserve' ).
    "! will render &lt;A xml:space="preserve"/>; as expected, there's no
    "! xmlns:xml="http://www.w3.org/XML/1998/namespace".
    METHODS attribute_xml_namespace FOR TESTING RAISING cx_static_check.
    METHODS most_simple_valid_xml FOR TESTING RAISING cx_static_check.
    METHODS namespace FOR TESTING RAISING cx_static_check.
    METHODS namespace_default FOR TESTING RAISING cx_static_check.
    METHODS namespace_default_by_attribute FOR TESTING RAISING cx_static_check.
    METHODS namespace_inheritance FOR TESTING RAISING cx_static_check.
    METHODS namespace_set_prefix FOR TESTING RAISING cx_static_check.
    METHODS object_oriented_rendering FOR TESTING RAISING cx_static_check.
    METHODS token_based_rendering FOR TESTING RAISING cx_static_check.
    METHODS write_namespace_declaration FOR TESTING RAISING cx_static_check.
    "! Order between namespace declarations, 1 then 2
    METHODS write_namespace_declaration_2 FOR TESTING RAISING cx_static_check.
    "! Order between namespace declarations, 2 then 1
    METHODS write_namespace_declaration_3 FOR TESTING RAISING cx_static_check.
    "! write_namespace_declaration called right before write_node( element )
    "! will position namespace declarations at the beginning of element,
    "! before default namespace and attributes
    METHODS write_namespace_declaration_4 FOR TESTING RAISING cx_static_check.
    "! write_namespace_declaration called right after write_node( element )
    "! will position namespace declarations at the end of element
    METHODS write_namespace_declaration_5 FOR TESTING RAISING cx_static_check.
    METHODS write_namespace_declaration_6 FOR TESTING RAISING cx_static_check.

    DATA open_element  TYPE REF TO if_sxml_open_element.
    DATA writer        TYPE REF TO if_sxml_writer.
    DATA string        TYPE string.
    DATA value_node    TYPE REF TO if_sxml_value_node.
    DATA close_element TYPE REF TO if_sxml_close_element.

    METHODS get_output
      IMPORTING
        io_writer        TYPE REF TO if_sxml_writer
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS setup.

ENDCLASS.


INTERFACE lif_wrap_ixml_all_friends.
ENDINTERFACE.


INTERFACE lif_wrap_ixml_istream.
  INTERFACES zif_excel_ixml_istream.
  DATA ixml_istream TYPE REF TO if_ixml_istream.
ENDINTERFACE.


INTERFACE lif_wrap_ixml_ostream.
  INTERFACES zif_excel_ixml_ostream.
  DATA ixml_ostream TYPE REF TO if_ixml_ostream.
ENDINTERFACE.


CLASS lth_wrap_ixml_unknown DEFINITION
    FOR TESTING
    CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_unknown.
    INTERFACES lif_wrap_ixml_all_friends.

ENDCLASS.


CLASS lth_wrap_ixml_node DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PROTECTED
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node.

  PRIVATE SECTION.

    DATA ixml_node TYPE REF TO if_ixml_node.

ENDCLASS.


CLASS lth_wrap_ixml DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml.

    TYPES:
      BEGIN OF ts_wrapped_ixml_object,
        ixml_object         TYPE REF TO object,
        ixml_object_wrapper TYPE REF TO object,
      END OF ts_wrapped_ixml_object.
    TYPES tt_wrapped_ixml_object TYPE HASHED TABLE OF ts_wrapped_ixml_object WITH UNIQUE KEY ixml_object.

    CLASS-DATA wrapped_ixml_objects TYPE tt_wrapped_ixml_object.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_excel_ixml.

    CLASS-METHODS wrap_ixml
      IMPORTING
        io_ixml_unknown TYPE REF TO if_ixml_unknown
      RETURNING
        VALUE(ro_result) TYPE REF TO object.

  PRIVATE SECTION.

    CLASS-DATA singleton TYPE REF TO lth_wrap_ixml.
    DATA ixml TYPE REF TO if_ixml.

ENDCLASS.


CLASS lth_wrap_ixml_attribute DEFINITION
    INHERITING FROM lth_wrap_ixml_node
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_attribute.

  PRIVATE SECTION.

    DATA ixml_attribute TYPE REF TO if_ixml_attribute.

ENDCLASS.


CLASS lth_wrap_ixml_character_data DEFINITION
    INHERITING FROM lth_wrap_ixml_node
    FOR TESTING
    CREATE PROTECTED
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_character_data.

ENDCLASS.


CLASS lth_wrap_ixml_document DEFINITION
    INHERITING FROM lth_wrap_ixml_node
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_document.

  PRIVATE SECTION.

    DATA ixml_document TYPE REF TO if_ixml_document.

ENDCLASS.


CLASS lth_wrap_ixml_element DEFINITION
    INHERITING FROM lth_wrap_ixml_node
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends
            ltc_ixml_isxml_element.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_element.

  PRIVATE SECTION.

    DATA ixml_element TYPE REF TO if_ixml_element.

ENDCLASS.


CLASS lth_wrap_ixml_encoding DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_encoding.

  PRIVATE SECTION.

    DATA ixml_encoding TYPE REF TO if_ixml_encoding.

ENDCLASS.


CLASS lth_wrap_ixml_istream_string DEFINITION
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_istream.

ENDCLASS.


CLASS lth_wrap_ixml_istream_xstring DEFINITION
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_istream.

ENDCLASS.


CLASS lth_wrap_ixml_named_node_map DEFINITION
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_named_node_map.

  PRIVATE SECTION.

    DATA ixml_named_node_map TYPE REF TO if_ixml_named_node_map.

ENDCLASS.


CLASS lth_wrap_ixml_node_collection DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_collection.

  PRIVATE SECTION.

    DATA ixml_node_collection TYPE REF TO if_ixml_node_collection.

ENDCLASS.


CLASS lth_wrap_ixml_node_iterator DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_iterator.

  PRIVATE SECTION.

    DATA ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

ENDCLASS.


CLASS lth_wrap_ixml_node_list DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_list.

  PRIVATE SECTION.

    DATA ixml_node_list TYPE REF TO if_ixml_node_list.

ENDCLASS.


CLASS lth_wrap_ixml_ostream_string DEFINITION
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_ostream.

ENDCLASS.


CLASS lth_wrap_ixml_ostream_xstring DEFINITION
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_wrap_ixml_ostream.

*  PRIVATE SECTION.
*
*    DATA ixml_ostream_xstring TYPE REF TO if_ixml_ostream.

ENDCLASS.


CLASS lth_wrap_ixml_parser DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_parser.

  PRIVATE SECTION.

    DATA ixml_parser TYPE REF TO if_ixml_parser.

ENDCLASS.


CLASS lth_wrap_ixml_renderer DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_renderer.

  PRIVATE SECTION.

    DATA ixml_renderer TYPE REF TO if_ixml_renderer.

ENDCLASS.


CLASS lth_wrap_ixml_stream DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream.

ENDCLASS.


CLASS lth_wrap_ixml_stream_factory DEFINITION
    INHERITING FROM lth_wrap_ixml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream_factory.

  PRIVATE SECTION.

    DATA ixml_stream_factory TYPE REF TO if_ixml_stream_factory.

ENDCLASS.


CLASS lth_wrap_ixml_text DEFINITION
    INHERITING FROM lth_wrap_ixml_character_data
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_wrap_ixml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_text.

  PRIVATE SECTION.

    DATA ixml_text TYPE REF TO if_ixml_text.

ENDCLASS.


CLASS lth_ixml_isxml DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA document       TYPE REF TO zif_excel_ixml_document.
    CLASS-DATA ixml           TYPE REF TO zif_excel_ixml.
    CLASS-DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.
    CLASS-METHODS parse
      IMPORTING
        ixml_or_isxml TYPE REF TO zif_excel_ixml
        xml_string TYPE csequence
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_excel_ixml_document.

    CLASS-METHODS render
      IMPORTING
        ixml_or_isxml TYPE REF TO zif_excel_ixml
      RETURNING
        VALUE(rv_result) TYPE string.

    CLASS-METHODS create_document.

ENDCLASS.


CLASS lth_ixml DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA document       TYPE REF TO if_ixml_document.
    CLASS-DATA encoding       TYPE REF TO if_ixml_encoding.
    CLASS-DATA ixml           TYPE REF TO if_ixml.
    CLASS-DATA stream_factory TYPE REF TO if_ixml_stream_factory.
    CLASS-METHODS parse
      IMPORTING
        xml_string TYPE csequence
      RETURNING
        VALUE(ro_result) TYPE REF TO if_ixml_document.

    CLASS-METHODS render
      RETURNING
        VALUE(rv_result) TYPE string.

    CLASS-METHODS create_document.

ENDCLASS.


CLASS ltc_diff_ixml_sxml_parser IMPLEMENTATION.
  METHOD missing_namespace_declaration.
    string = '<a:A><B/></a:A>'.
    xstring = cl_abap_codepage=>convert_to( string ).
    reader = cl_sxml_string_reader=>create( xstring ).
    TRY.
        node = reader->read_next_node( ).
        cl_abap_unit_assert=>fail( msg = 'missing namespace declaration xmlns:a="..." -> should have failed' ).
      CATCH cx_sxml_parse_error.
        cl_abap_unit_assert=>assert_not_bound( act = node ).
        cl_abap_unit_assert=>assert_equals( act = reader->name
                                            exp = '' ).
    ENDTRY.
    open_element ?= reader->read_next_node( ).
    cl_abap_unit_assert=>assert_equals( act = open_element->qname-name
                                        exp = 'B' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml IMPLEMENTATION.
ENDCLASS.


CLASS ltc_ixml_element IMPLEMENTATION.
  METHOD get_attribute_xmlns.
    " Method READ_THEME of class ZCL_EXCEL_THEME:
    "    CONSTANTS c_theme_xmlns TYPE string VALUE 'xmlns:a'.    "#EC NOTEXT
    "      xmls_a = lo_node_theme->get_attribute( name = c_theme_xmlns ).
    "    In fact, xmls_a IS NOT USED, it can be deleted!
    " GET_ATTRIBUTE with NAME = 'xmlns:XXXX' always returns no attribute found!
    document = lth_ixml=>parse( '<A xmlns:nsprefix="nsuri" nsprefix:attr="A1"/>' ).
    element = document->get_root_element( ).
    string = element->get_attribute( name = 'xmlns:nsprefix' ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '' ).
  ENDMETHOD.

  METHOD set_attribute_ns_name_xmlns.
    " Two ways to set attributes from a namespace
    document = lth_ixml=>parse( '<A/>' ).
    element = document->get_root_element( ).
    element->set_attribute_ns( name  = 'xmlns:nsprefix'
                               value = 'nsuri' ).
    element->set_attribute_ns( name  = 'nsprefix:attr'
                               value = 'A1' ).
    string = lth_ixml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xmlns:nsprefix="nsuri" nsprefix:attr="A1"/>' ).

    " TODO: abaplint deps is missing parameter uri of set_attribute_ns
    document = lth_ixml=>parse( '<A/>' ).
    element = document->get_root_element( ).
    element->set_attribute_ns( name   = 'nsprefix'
                               prefix = 'xmlns'
                               uri    = ''
                               value  = 'nsuri' ).
    element->set_attribute_ns( name   = 'attr'
                               prefix = 'nsprefix'
                               uri    = 'nsuri'
                               value  = 'A1' ).
    string = lth_ixml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xmlns:nsprefix="nsuri" nsprefix:attr="A1"/>' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_node IMPLEMENTATION.
  METHOD append_child.
    " Append to the root element is not the same as append to the document.

    DATA lo_element TYPE REF TO if_ixml_element.

    document = lth_ixml=>parse( '<A/>' ).
    element = document->get_root_element( ).
    lo_element = document->create_element( name = 'B' ).
    element->append_child( lo_element ).
    string = lth_ixml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A><B/></A>' ).

    document = lth_ixml=>parse( '<A/>' ).
    element = document->get_root_element( ).
    lo_element = document->create_element( name = 'B' ).
    document->append_child( lo_element ).
    string = lth_ixml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A/><B/>' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_isxml IMPLEMENTATION.
  METHOD create_encoding.
*ZCL_EXCEL_THEME
*    lo_encoding = lo_ixml->create_encoding( byte_order = if_ixml_encoding=>co_platform_endian
*                                            character_set = 'UTF-8' ).
*    lo_document = lo_ixml->create_document( ).
*    lo_document->set_encoding( lo_encoding ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      encoding = ixml_or_isxml->create_encoding( byte_order    = if_ixml_encoding=>co_platform_endian
                                                 character_set = 'UTF-8' ).
      document->set_encoding( encoding ).
      stream_factory = ixml_or_isxml->create_stream_factory( ).
      GET REFERENCE OF xstring INTO ref_xstring.
      ostream = stream_factory->create_ostream_xstring( ref_xstring ).
      renderer = ixml_or_isxml->create_renderer( ostream  = ostream
                                                 document = document ).
      document->create_simple_element( name   = 'é'
                                       parent = document ).
      CLEAR xstring.
      rc = renderer->render( ).
      string = cl_abap_codepage=>convert_from( xstring ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<?xml version="1.0" encoding="utf-8"?><é/>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_ixml_and_isxml.
    ixml = lth_wrap_ixml=>create( ).
    INSERT ixml INTO TABLE rt_result.
    isxml = zcl_excel_ixml=>create( ).
    INSERT isxml INTO TABLE rt_result.
  ENDMETHOD.

  METHOD render.
    DATA lr_string   TYPE REF TO string.
    DATA lo_ostream  TYPE REF TO zif_excel_ixml_ostream.
    DATA lo_renderer TYPE REF TO zif_excel_ixml_renderer.

    stream_factory = ixml_or_isxml->create_stream_factory( ).
    GET REFERENCE OF rv_result INTO lr_string.
    lo_ostream = stream_factory->create_ostream_cstring( lr_string ).
    lo_renderer = ixml_or_isxml->create_renderer( ostream  = lo_ostream
                                                  document = document ).
    document->set_declaration( abap_false ).
    " Fills RV_RESULT
    lo_renderer->render( ).
    " remove the UTF-16 BOM (i.e. remove the first character)
    SHIFT rv_result LEFT BY 1 PLACES.

    " Normalize XML according to SXML limitations in order to compare IXML
    " and SXML results by simple string comparison.
    rv_result = lcl_rewrite_xml_via_sxml=>execute( rv_result ).
  ENDMETHOD.

  METHOD setup.
    ixml_and_isxml = get_ixml_and_isxml( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_isxml_document IMPLEMENTATION.
  METHOD create_element.
* (only at 2 places in test classes)
* Method SET_CELL of local class LTC_COLUMN_FORMULA of class ZCL_EXCEL_WRITER_2007.
*     lo_cell = lo_document->create_element( 'c' ).
*     lo_cell->set_attribute( name = 'r' value = |R{ is_cell_data-cell_row }C{ is_cell_data-cell_column }| ).
*     lo_root->append_child( lo_cell ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      element = document->create_element( name = 'A' ).
      document->append_child( element ).
      string = render( ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<A/>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_simple_element.
* Method ADD_HYPERLINKS of local class LCL_CREATE_XL_SHEET of class ZCL_EXCEL_WRITER_2007.
*      lo_element = o_document->create_simple_element( name   = 'hyperlinks'
*                                                      parent = o_document ).
*        lo_element_2 = o_document->create_simple_element( name   = 'hyperlink'
*                                                          parent = lo_element ).
*        lo_element_2->set_attribute_ns( name  = 'ref'
*                                        value = lv_value ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      document->create_simple_element( name   = 'A'
                                       parent = document ).
      string = render( ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<A/>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_simple_element_ns.
* Method WRITE_THEME of class ZCL_EXCEL_THEME
*    CONSTANTS c_theme TYPE string VALUE 'theme'.            "#EC NOTEXT
*    CONSTANTS c_theme_xmlns TYPE string VALUE 'xmlns:a'.    "#EC NOTEXT
*    CONSTANTS c_theme_prefix TYPE string VALUE 'a'.         "#EC NOTEXT
*    CONSTANTS c_theme_xmlns_val TYPE string VALUE 'http://schemas.openxmlformats.org/drawingml/2006/main'. "#EC NOTEXT
*    lo_document->set_namespace_prefix( prefix = 'a' ).
*    lo_element_root = lo_document->create_simple_element_ns( prefix = c_theme_prefix
*                                                             name   = c_theme
*                                                             parent = lo_document ).
*    lo_element_root->set_attribute_ns( name  = c_theme_xmlns
*                                       value = c_theme_xmlns_val ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      document->set_namespace_prefix( prefix = 'a' ).
      element = document->create_simple_element_ns( name   = 'A'
                                                    parent = document
                                                    prefix = 'a' ).
      element->set_attribute_ns( name  = 'xmlns:a'
                                 value = 'http://schemas.openxmlformats.org/drawingml/2006/main' ).
      string = render( ).
      cl_abap_unit_assert=>assert_equals(
          act = string
          exp = `<a:A xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"/>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_from_name.
*Method LOAD_WORKSHEET_TABLES of ZCL_EXCEL_READER_2007
*    lo_ixml_table_style ?= lo_ixml_table->find_from_name( 'tableStyleInfo' ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = |<A><B/></A>| ).
      element = document->find_from_name( name = 'B' ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = 'B' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_from_name_ns.
* Method LOAD_CHART_ATTRIBUTES of class ZCL_EXCEL_DRAWING
*     CONSTANTS: BEGIN OF namespace,
*                  c   TYPE string VALUE 'http://schemas.openxmlformats.org/drawingml/2006/chart',
*                END OF namespace.
*     node ?= ip_chart->if_ixml_node~get_first_child( ).
*     node2 ?= node->find_from_name_ns( name = 'date1904' uri = namespace-c ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = |<A><B>B1</B><a:B xmlns:a="a">B2</a:B></A>| ).
      element = document->find_from_name_ns( name = 'B'
                                             uri  = 'a' ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = 'B' ).
      cl_abap_unit_assert=>assert_equals( act = element->get_value( )
                                          exp = 'B2' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_elements_by_tag_name.
* Method CREATE_CONTENT_TYPES of class ZCL_EXCEL_WRITER_XLSM
*    lo_collection = lo_document->get_elements_by_tag_name( 'Override' ).

    DATA lo_isxml_node_collection TYPE REF TO zif_excel_ixml_node_collection.
    DATA lo_isxml_node_iterator   TYPE REF TO zif_excel_ixml_node_iterator.
    DATA lo_isxml_node            TYPE REF TO zif_excel_ixml_node.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = |<A><B>B1</B><a:B xmlns:a="a">B2</a:B><B>B3</B></A>| ).
      lo_isxml_node_collection = document->get_elements_by_tag_name( 'B' ).
      lo_isxml_node_iterator = lo_isxml_node_collection->create_iterator( ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_isxml_node->get_value( )
                                          exp = 'B1' ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_isxml_node->get_value( )
                                          exp = 'B3' ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = lo_isxml_node ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_root_element.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = |<A><B>B1</B></A>| ).
      element = document->get_root_element( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = 'A' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_encoding.
* Method CREATE_XML_DOCUMENT of class ZCL_EXCEL_WRITER_2007:
*    DATA lo_encoding TYPE REF TO if_ixml_encoding.
*    lo_encoding = me->ixml->create_encoding( byte_order = if_ixml_encoding=>co_platform_endian
*                                             character_set = 'utf-8' ).
*    ro_document = me->ixml->create_document( ).
*    ro_document->set_encoding( lo_encoding ).

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      encoding = ixml_or_isxml->create_encoding( byte_order    = if_ixml_encoding=>co_platform_endian
                                                 character_set = 'utf-8' ).
      document->set_encoding( encoding ).
      element = document->create_simple_element( name   = 'ROOT'
                                                 parent = document ).
      stream_factory = ixml_or_isxml->create_stream_factory( ).
      GET REFERENCE OF xstring INTO ref_xstring.
      ostream = stream_factory->create_ostream_xstring( ref_xstring ).
      renderer = ixml_or_isxml->create_renderer( ostream  = ostream
                                                 document = document ).
      CLEAR xstring.
      rc = renderer->render( ).
      string = cl_abap_codepage=>convert_from( xstring ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = '<?xml version="1.0" encoding="utf-8"?><ROOT/>' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_standalone.
* Method CREATE_XML_DOCUMENT of class ZCL_EXCEL_WRITER_2007:
*    DATA lo_encoding TYPE REF TO if_ixml_encoding.
*    lo_encoding = me->ixml->create_encoding( byte_order = if_ixml_encoding=>co_platform_endian
*                                             character_set = 'utf-8' ).
*    ro_document = me->ixml->create_document( ).
*    ro_document->set_encoding( lo_encoding ).
*    ro_document->set_standalone( abap_true ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
*         WHERE table_line = isxml
         .
      document = ixml_or_isxml->create_document( ).
      encoding = ixml_or_isxml->create_encoding( byte_order    = if_ixml_encoding=>co_platform_endian
                                                 character_set = 'utf-8' ).
      document->set_encoding( encoding ).
      document->set_standalone( abap_true ).
      element = document->create_simple_element( name   = 'ROOT'
                                                 parent = document ).
      stream_factory = ixml_or_isxml->create_stream_factory( ).
      GET REFERENCE OF xstring INTO ref_xstring.
      ostream = stream_factory->create_ostream_xstring( ref_xstring ).
      renderer = ixml_or_isxml->create_renderer( ostream  = ostream
                                                 document = document ).
      CLEAR xstring.
      rc = renderer->render( ).
      string = cl_abap_codepage=>convert_from( xstring ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = '<?xml version="1.0" encoding="utf-8" standalone="yes"?><ROOT/>' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD setup.
    ixml_and_isxml = get_ixml_and_isxml( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_isxml_element IMPLEMENTATION.
  METHOD find_from_name_level_1.
* Method LOAD_WORKSHEET_TABLES of class ZCL_EXCEL_READER_2007:
*    DATA lo_ixml_table TYPE REF TO if_ixml_element.
*      lo_ixml_table_style ?= lo_ixml_table->find_from_name( 'tableStyleInfo' ).

    DATA lo_element TYPE REF TO zif_excel_ixml_element.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
*         WHERE table_line = isxml
.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = |<A xmlns:a="a"><C>C1</C><B><a:C>C2</a:C><C>C3</C></B></A>| ).
      lo_element = document->get_root_element( ).
      lo_element ?= lo_element->get_first_child( ).
      lo_element ?= lo_element->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_element->get_name( )
                                          exp = 'B' ).
      element = lo_element->find_from_name( name = 'C' ).
      cl_abap_unit_assert=>assert_equals( act = element->get_value( )
                                          exp = 'C3' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_from_name_level_2.
* Method LOAD_WORKSHEET_TABLES of class ZCL_EXCEL_READER_2007:
*    DATA lo_ixml_table TYPE REF TO if_ixml_element.
*      lo_ixml_table_style ?= lo_ixml_table->find_from_name( 'tableStyleInfo' ).

    DATA lo_element TYPE REF TO zif_excel_ixml_element.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse(
                     ixml_or_isxml = ixml_or_isxml
                     xml_string    = |<A xmlns:a="a"><C>C1</C><B><a:C>C2</a:C><D><C>C3</C></D></B></A>| ).
      lo_element = document->get_root_element( ).
      lo_element ?= lo_element->get_first_child( ).
      lo_element ?= lo_element->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_element->get_name( )
                                          exp = 'B' ).
      element = lo_element->find_from_name( name = 'C' ).
      cl_abap_unit_assert=>assert_equals( act = element->get_value( )
                                          exp = 'C3' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_from_name_ns.
* Method LOAD_CHART_ATTRIBUTES of class ZCL_EXCEL_DRAWING:
*    DATA: node                TYPE REF TO if_ixml_element.
*    node2 ?= node->find_from_name_ns( name = 'date1904' uri = namespace-c ).
*        node2 ?= node->find_from_name_ns( name = 'marker' uri = namespace-c depth = '1' ).

    DATA lo_element TYPE REF TO zif_excel_ixml_element.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml
         WHERE table_line = ixml
*         WHERE table_line = isxml
.
      document = lth_ixml_isxml=>parse(
                     ixml_or_isxml = ixml_or_isxml
                     xml_string    = |<A xmlns:pc="uc"><B><C>C1</C><D><C>C2</C><pc:C>C3</pc:C></D></B></A>| ).
      lo_element = document->get_root_element( ).
      lo_element ?= lo_element->get_first_child( ).
*      lo_element ?= lo_element->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_element->get_name( )
                                          exp = 'B' ).
      element = lo_element->find_from_name_ns( depth = iv_depth
                                               name  = 'C'
                                               uri   = 'uc' ).
      IF iv_exp_found = abap_false.
        cl_abap_unit_assert=>assert_not_bound( element ).
      ELSE.
        cl_abap_unit_assert=>assert_equals( act = element->get_value( )
                                            exp = iv_exp_value ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_from_name_ns_depth_0.
    find_from_name_ns( iv_depth     = 0
                       iv_exp_found = abap_true
                       iv_exp_value = 'C3' ).
  ENDMETHOD.

  METHOD find_from_name_ns_depth_1.
    find_from_name_ns( iv_depth     = 1
                       iv_exp_found = abap_false ).
  ENDMETHOD.

  METHOD find_from_name_ns_depth_2.
    find_from_name_ns( iv_depth     = 2
                       iv_exp_found = abap_true
                       iv_exp_value = 'C3' ).
  ENDMETHOD.

  METHOD get_attribute.
    " Method LOAD_STYLE_BORDERS of class ZCL_EXCEL_READER_2007:
    "      IF lo_node_border->get_attribute( 'diagonalDown' ) IS NOT INITIAL.
    " Method READ_THEME of class ZCL_EXCEL_THEME:
    "    CONSTANTS c_theme_xmlns TYPE string VALUE 'xmlns:a'.    "#EC NOTEXT
    "      xmls_a = lo_node_theme->get_attribute( name = c_theme_xmlns ).
    " NB: above GET_ATTRIBUTE with NAME = 'xmlns:XXXX' always returns no attribute found, it's also not used
    "     so cleanup done -> https://github.com/abap2xlsx/abap2xlsx/pull/1183.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse(
                     ixml_or_isxml = ixml_or_isxml
                     xml_string    = `<A xmlns="default" xmlns:nsprefix="nsuri" nsprefix:attr="A1" attr="A2"/>` ).
      element = document->get_root_element( ).
      string = element->get_attribute( name = 'xmlns:nsprefix' ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = '' ).
      string = element->get_attribute( name = 'nsprefix:attr' ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = '' ).
      string = element->get_attribute( name = 'attr' ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = 'A2' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_attribute_node_ns.
* Method LOAD_COMMENTS of class ZCL_EXCEL_READER_2007:
*      lo_attr = lo_node_comment->get_attribute_node_ns( name = 'ref' ).
*      lv_attr_value  = lo_attr->get_value( ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = `<A xmlns:nsprefix="nsuri" nsprefix:attr="A1" attr="A2"/>` ).
      element = document->get_root_element( ).
      attribute = element->get_attribute_node_ns( name = 'attr'
                                                  uri  = 'nsuri' ).
      cl_abap_unit_assert=>assert_equals( act = attribute->get_value( )
                                          exp = 'A1' ).
      attribute = element->get_attribute_node_ns( name = 'attr' ).
      cl_abap_unit_assert=>assert_equals( act = attribute->get_value( )
                                          exp = 'A2' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_attribute_ns.
* Method LOAD_WORKSHEET_HYPERLINKS of class ZCL_EXCEL_READER_2007:
*     ls_hyperlink-tooltip  = lo_ixml_hyperlink->get_attribute_ns( 'tooltip' ).
*     ls_hyperlink-r_id     = lo_ixml_hyperlink->get_attribute_ns( name = 'id' uri = namespace-r ).

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = `<A xmlns:nsprefix="nsuri" nsprefix:attr="A1" attr="A2"/>` ).
      element = document->get_root_element( ).
      string = element->get_attribute_ns( name = 'nsprefix'
                                          uri  = 'xmlns' ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = '' ).
      string = element->get_attribute_ns( name = 'attr'
                                          uri  = 'nsuri' ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = 'A1' ).
      string = element->get_attribute_ns( name = 'attr' ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = 'A2' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_elements_by_tag_name.
* Method LOAD_WORKSHEET_TABLES of class ZCL_EXCEL_READER_2007:
*      lo_ixml_table_columns =  lo_ixml_table->get_elements_by_tag_name( name = 'tableColumn' ).

    DATA lo_isxml_node_collection TYPE REF TO zif_excel_ixml_node_collection.
    DATA lo_isxml_node_iterator   TYPE REF TO zif_excel_ixml_node_iterator.
    DATA lo_isxml_node            TYPE REF TO zif_excel_ixml_node.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse(
          ixml_or_isxml = ixml_or_isxml
          xml_string    = |<A><B>B1</B><a:B xmlns:a="a">B2</a:B><B>B3</B><C><B>B4</B><a:B xmlns:a="a">B5</a:B><B>B6</B></C></A>| ).
      element = document->find_from_name( name = 'C' ).
      lo_isxml_node_collection = element->get_elements_by_tag_name( 'B' ).
      lo_isxml_node_iterator = lo_isxml_node_collection->create_iterator( ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_isxml_node->get_value( )
                                          exp = 'B4' ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_isxml_node->get_value( )
                                          exp = 'B6' ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = lo_isxml_node ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_elements_by_tag_name_ns.
* Method LOAD_DXF_STYLES of class ZCL_EXCEL_READER_2007:
*    lo_nodes_dxf ?= lo_node_dxfs->get_elements_by_tag_name_ns( name = 'dxf' uri = namespace-main ).

    DATA lo_isxml_node_collection TYPE REF TO zif_excel_ixml_node_collection.
    DATA lo_isxml_node_iterator   TYPE REF TO zif_excel_ixml_node_iterator.
    DATA lo_isxml_node            TYPE REF TO zif_excel_ixml_node.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse(
                     ixml_or_isxml = ixml_or_isxml
                     xml_string    = |<A xmlns:a="nsa"><B>B1</B><a:B>B2</a:B><B>B3</B><a:B>B4</a:B></A>| ).
      element = document->get_root_element( ).
      lo_isxml_node_collection = element->get_elements_by_tag_name_ns( name = 'B'
                                                                       uri  = 'nsa' ).
      lo_isxml_node_iterator = lo_isxml_node_collection->create_iterator( ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_isxml_node->get_value( )
                                          exp = 'B2' ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_isxml_node->get_value( )
                                          exp = 'B4' ).
      lo_isxml_node = lo_isxml_node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = lo_isxml_node ).
    ENDLOOP.
  ENDMETHOD.

  METHOD remove_attribute_ns.
* Method CREATE_CONTENT_TYPES of class ZCL_EXCEL_WRITER_XLSM:
*        lo_element->remove_attribute_ns( lc_xml_attr_contenttype ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse(
                     ixml_or_isxml = ixml_or_isxml
                     xml_string    = `<A xmlns="nsuri" xmlns:nsprefix="nsuri2" nsprefix:attr="A1" attr="A2"/>` ).
      element = document->get_root_element( ).
      element->remove_attribute_ns( name = 'attr' ).
      string = render( ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<A nsprefix:attr="A1" xmlns="nsuri" xmlns:nsprefix="nsuri2"/>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_attribute.
* Method CREATE_XL_SHAREDSTRINGS of class ZCL_EXCEL_WRITER_2007:
*           lo_sub_element->set_attribute( name = 'space' namespace = 'xml' value = 'preserve' ).
* Method CREATE_XL_SHEET_COLUMN_FORMULA of class ZCL_EXCEL_WRITER_2007:
*      eo_element->set_attribute( name  = 't'
*                                 value = <ls_column_formula_used>-t ).

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      element = document->create_simple_element( name   = 'A'
                                                 parent = document ).
      element->set_attribute( name  = 'a'
                              value = '1' ).
      element->set_attribute( name      = 'space'
                              namespace = 'xml'
                              value     = 'preserve' ).
      string = render( ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<A a="1" xml:space="preserve"/>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_attribute_ns.
* Method CREATE_CONTENT_TYPES of class ZCL_EXCEL_WRITER_XLSM:
*        lo_element->set_attribute_ns( name  = lc_xml_attr_contenttype
*                                      value = lc_xml_node_workb_ct ).
* Method CLONE_IXML_WITH_NAMESPACES of class ZCL_EXCEL_COMMON:
*      result->set_attribute_ns( prefix = 'xmlns' name = <xmlns>-name value = <xmlns>-value ).
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      document->set_namespace_prefix( prefix = 'a' ).
      element = document->create_simple_element_ns( name   = 'A'
                                                    parent = document
                                                    prefix = 'a' ).
      element->set_attribute_ns( name  = 'xmlns:a'
                                 value = 'nsuri_a' ).
      document->set_namespace_prefix( prefix = 'b' ).
      element = document->create_simple_element_ns( name   = 'B'
                                                    parent = element
                                                    prefix = 'b' ).
      element->set_attribute_ns( name   = 'b'
                                 prefix = 'xmlns'
                                 value  = 'nsuri_b' ).
      string = render( ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<a:A xmlns:a="nsuri_a"><b:B xmlns:b="nsuri_b"/></a:A>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD setup.
    ixml_and_isxml = get_ixml_and_isxml( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_isxml_node IMPLEMENTATION.
  METHOD append_child.
* Method ADD_1_VAL_CHILD_NODE of class ZCL_EXCEL_WRITER_2007:
*    io_parent->append_child( new_child = lo_child ).

    DATA lo_element TYPE REF TO zif_excel_ixml_element.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<ROOT/>' ).
      element = document->get_root_element( ).
      lo_element = document->create_element( name = 'A' ).
      element->append_child( lo_element ).
      string = lth_ixml_isxml=>render( ixml_or_isxml ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<ROOT><A/></ROOT>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD clone.
* Method CLONE_IXML_WITH_NAMESPACES of class ZCL_EXCEL_COMMON:
*    result ?= element->clone( ).

    DATA lo_element TYPE REF TO zif_excel_ixml_element.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = `<ROOT><A/></ROOT>` ).
      element ?= document->get_root_element( ).
      lo_element ?= element->get_first_child( )->clone( ).
      element->append_child( lo_element ).
      string = lth_ixml_isxml=>render( ixml_or_isxml ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `<ROOT><A/><A/></ROOT>` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_iterator.
* Method CLONE_IXML_WITH_NAMESPACES of class ZCL_EXCEL_COMMON:
*    iterator = element->create_iterator( ).
*    node = iterator->get_next( ).
*    WHILE node IS BOUND.
*      node = iterator->get_next( ).
*    ENDWHILE.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = `<A><B><C/></B><D/></A>` ).
      element ?= document->get_root_element( ).
      node_iterator = element->create_iterator( ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `A` ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `B` ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `C` ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `D` ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = element ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = element ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_attributes.
* Method FILL_STRUCT_FROM_ATTRIBUTES of class ZCL_EXCEL_READER_2007:
*    lo_attributes  = ip_element->get_attributes( ).
*    lo_iterator    = lo_attributes->create_iterator( ).
*    lo_attribute  ?= lo_iterator->get_next( ).
*    WHILE lo_attribute IS BOUND.
*      lo_attribute ?= lo_iterator->get_next( ).
*    ENDWHILE.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = `<A a="1" b="2"/>` ).
      element ?= document->get_root_element( ).
      named_node_map = element->get_attributes( ).
      node_iterator = named_node_map->create_iterator( ).
      attribute ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = attribute->get_name( )
                                          exp = `a` ).
      cl_abap_unit_assert=>assert_equals( act = attribute->get_value( )
                                          exp = `1` ).
      attribute ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = attribute->get_name( )
                                          exp = `b` ).
      cl_abap_unit_assert=>assert_equals( act = attribute->get_value( )
                                          exp = `2` ).
      attribute ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = attribute ).
      attribute ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = attribute ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_children.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<A><B><C/></B><D/></A>' ).
      " WHEN
      element = document->get_root_element( ).
      node_list = element->get_children( ).
      node_iterator = node_list->create_iterator( ).
      " THEN
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `B` ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `D` ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = element ).
      element ?= node_iterator->get_next( ).
      cl_abap_unit_assert=>assert_not_bound( act = element ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_first_child.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<A><B/></A>' ).
      " WHEN
      element ?= document->get_first_child( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `A` ).
      " WHEN
      element ?= element->get_first_child( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = `B` ).
      " WHEN
      element ?= element->get_first_child( ).
      " THEN
      cl_abap_unit_assert=>assert_not_bound( act = element ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_name.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<A a="1"/>' ).
      element ?= document->get_root_element( ).
      node = element->get_attribute_node_ns( name = 'a' ).
      " WHEN
      string = node->get_name( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `a` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_namespace_prefix.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<a:A xmlns:a="nsuri"/>' ).
      node = document->get_root_element( ).
      " WHEN
      string = node->get_namespace_prefix( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `a` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_namespace_uri.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<a:A xmlns:a="nsuri"/>' ).
      node = document->get_root_element( ).
      " WHEN
      string = node->get_namespace_uri( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = `nsuri` ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_next.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<A><B><C/></B><D><E/></D><F/></A>' ).
      node = document->get_root_element( )->get_first_child( ).
      " WHEN
      node = node->get_next( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = node->get_name( )
                                          exp = `D` ).
      " WHEN
      node = node->get_next( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = node->get_name( )
                                          exp = `F` ).
      " WHEN
      node = node->get_next( ).
      " THEN
      cl_abap_unit_assert=>assert_not_bound( act = node ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_type.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<A a="1"/>' ).
      " WHEN
      node = document.
      type = node->get_type( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = type
                                          exp = zif_excel_ixml_node=>co_node_document ).
      " WHEN
      element = document->get_root_element( ).
      node = element.
      type = node->get_type( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = type
                                          exp = zif_excel_ixml_node=>co_node_element ).
      " WHEN
      node = element->get_attribute_node_ns( name = 'a' ).
      type = node->get_type( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = type
                                          exp = zif_excel_ixml_node=>co_node_attribute ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_value.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      " GIVEN
      document = lth_ixml_isxml=>parse( ixml_or_isxml = ixml_or_isxml
                                        xml_string    = '<A>1<B>2</B>3</A>' ).
      " WHEN
      node = document.
      value = node->get_value( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = value
                                          exp = '' ).
      " WHEN
      node = document->get_root_element( ).
      value = node->get_value( ).
      " THEN
      cl_abap_unit_assert=>assert_equals( act = value
                                          exp = '123' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_namespace_prefix.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
*         WHERE table_line = isxml
.
    ENDLOOP.
*  element = document->set_namespace_prefix( ).
    cl_abap_unit_assert=>fail( msg = 'Not tested yet' ).
  ENDMETHOD.

  METHOD set_value.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
*         WHERE table_line = isxml
.
    ENDLOOP.
    cl_abap_unit_assert=>fail( msg = 'Not tested yet' ).
  ENDMETHOD.

  METHOD setup.
    ixml_and_isxml = get_ixml_and_isxml( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_isxml_parser IMPLEMENTATION.
  METHOD add_strip_space_element.
* Method GET_IXML_FROM_ZIP_ARCHIVE of class ZCL_EXCEL_READER_2007:
*    lo_ixml           = cl_ixml=>create( ).
*    lo_streamfactory  = lo_ixml->create_stream_factory( ).
*    lo_istream        = lo_streamfactory->create_istream_xstring( lv_content ).
*    r_ixml            = lo_ixml->create_document( ).
*    lo_parser         = lo_ixml->create_parser( stream_factory = lo_streamfactory
*                                                istream        = lo_istream
*                                                document       = r_ixml ).
*    lo_parser->set_normalizing( is_normalizing ).
*    lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
*    lo_parser->parse( ).
* Method PARSE_STRING of class ZCL_EXCEL_THEME_FMT_SCHEME:
*    li_ixml = cl_ixml=>create( ).
*    li_document = li_ixml->create_document( ).
*    li_factory = li_ixml->create_stream_factory( ).
*    li_istream = li_factory->create_istream_string( iv_string ).
*    li_parser = li_ixml->create_parser(
*      stream_factory = li_factory
*      istream        = li_istream
*      document       = li_document ).
*    li_parser->add_strip_space_element( ).
*    li_parser->parse( ).
*    li_istream->close( ).
*    ri_node = li_document->get_first_child( ).
    cl_abap_unit_assert=>fail( msg = 'Not tested yet' ).
  ENDMETHOD.

  METHOD namespace.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
         WHERE table_line = isxml.
      " <A xmlns:nsprefix="nsuri" nsprefix:attr="B">  attr  nsuri      nsprefix
      " <A attr="B">                                  attr  (empty)    (empty)
      " <nsprefix:A xmlns:nsprefix="nsuri" attr="B">  attr  nsuri      nsprefix
      document = parse(
          `<nsprefix:A xmlns="dnsuri" xmlns:nsprefix="nsuri" nsprefix:attr="1" attr="2"><B attr="3"/></nsprefix:A>` ).
*      cl_abap_unit_assert=>assert_equals( act = rc
*                                          exp = lcl_isxml=>ixml_mr-dom_ok ).
*      element ?= document->get_first_child( ).
    ENDLOOP.
    cl_abap_unit_assert=>fail( msg = 'Not tested yet' ).
  ENDMETHOD.

  METHOD parse.
* Method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007
*    lo_parser->parse( ).
    xstring = cl_abap_codepage=>convert_to( iv_string ).
    stream_factory = ixml_or_isxml->create_stream_factory( ).
    istream = stream_factory->create_istream_xstring( xstring ).
    document = ixml_or_isxml->create_document( ).
    parser = ixml_or_isxml->create_parser( stream_factory = stream_factory
                                           istream        = istream
                                           document       = document ).
    parser->parse( ).
    ro_document = document.
  ENDMETHOD.

  METHOD setup.
    ixml_and_isxml = get_ixml_and_isxml( ).
  ENDMETHOD.

  METHOD set_normalizing.
* Method GET_IXML_FROM_ZIP_ARCHIVE of class ZCL_EXCEL_READER_2007:
*    METHODS get_ixml_from_zip_archive
*      IMPORTING
*        !i_filename     TYPE string
*        !is_normalizing TYPE abap_bool DEFAULT 'X'
*  METHOD get_ixml_from_zip_archive.
*    lo_ixml           = cl_ixml=>create( ).
*    lo_streamfactory  = lo_ixml->create_stream_factory( ).
*    lo_istream        = lo_streamfactory->create_istream_xstring( lv_content ).
*    r_ixml            = lo_ixml->create_document( ).
*    lo_parser         = lo_ixml->create_parser( stream_factory = lo_streamfactory
*                                                istream        = lo_istream
*                                                document       = r_ixml ).
*    lo_parser->set_normalizing( is_normalizing ).
*    lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
*    lo_parser->parse( ).
* All calls don't pass IS_NORMALIZING except the call in the method LOAD_SHARED_STRINGS of ZCL_EXCEL_READER_2007:
*    lo_shared_strings_xml = me->get_ixml_from_zip_archive( i_filename     = ip_path
*                                                           is_normalizing = space ).  " NO!!! normalizing - otherwise leading blanks will be omitted and that is not really desired for the stringtable
    cl_abap_unit_assert=>fail( msg = 'Not tested yet' ).
  ENDMETHOD.

  METHOD set_validating.
* Method GET_IXML_FROM_ZIP_ARCHIVE of class ZCL_EXCEL_READER_2007:
*    lo_ixml           = cl_ixml=>create( ).
*    lo_streamfactory  = lo_ixml->create_stream_factory( ).
*    lo_istream        = lo_streamfactory->create_istream_xstring( lv_content ).
*    r_ixml            = lo_ixml->create_document( ).
*    lo_parser         = lo_ixml->create_parser( stream_factory = lo_streamfactory
*                                                istream        = lo_istream
*                                                document       = r_ixml ).
*    lo_parser->set_normalizing( is_normalizing ).
*    lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
*    lo_parser->parse( ).
* Method PARSE_STRING of class ZCL_EXCEL_THEME_FMT_SCHEME:
*    li_ixml = cl_ixml=>create( ).
*    li_document = li_ixml->create_document( ).
*    li_factory = li_ixml->create_stream_factory( ).
*    li_istream = li_factory->create_istream_string( iv_string ).
*    li_parser = li_ixml->create_parser(
*      stream_factory = li_factory
*      istream        = li_istream
*      document       = li_document ).
*    li_parser->add_strip_space_element( ).
*    li_parser->parse( ).
*    li_istream->close( ).
*    ri_node = li_document->get_first_child( ).
    cl_abap_unit_assert=>fail( msg = 'Not tested yet' ).
  ENDMETHOD.

  METHOD several_children.
    DATA lo_element TYPE REF TO zif_excel_ixml_element.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      parse( `<A>T<B>U</B><C/></A>` ).
      cl_abap_unit_assert=>assert_equals( act = rc
                                          exp = lcl_isxml=>ixml_mr-dom_ok ).
      element ?= document->get_first_child( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = 'A' ).
      text ?= element->get_first_child( ).
      cl_abap_unit_assert=>assert_equals( act = text->get_value( )
                                          exp = 'T' ).
      lo_element ?= text->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_element->get_name( )
                                          exp = 'B' ).
      text ?= lo_element->get_first_child( ).
      cl_abap_unit_assert=>assert_equals( act = text->get_value( )
                                          exp = 'U' ).
      lo_element ?= lo_element->get_next( ).
      cl_abap_unit_assert=>assert_equals( act = lo_element->get_name( )
                                          exp = 'C' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD text_node.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      parse( `<A>B</A>` ).
      cl_abap_unit_assert=>assert_equals( act = rc
                                          exp = lcl_isxml=>ixml_mr-dom_ok ).
      element ?= document->get_first_child( ).
      cl_abap_unit_assert=>assert_equals( act = element->get_name( )
                                          exp = 'A' ).
      text ?= element->get_first_child( ).
      cl_abap_unit_assert=>assert_equals( act = text->get_value( )
                                          exp = 'B' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD two_ixml_encodings.
    DATA lo_encoding TYPE REF TO zif_excel_ixml_encoding.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      encoding = ixml_or_isxml->create_encoding( byte_order    = if_ixml_encoding=>co_little_endian
                                                 character_set = 'UTF-8' ).
      lo_encoding = ixml_or_isxml->create_encoding( byte_order    = if_ixml_encoding=>co_little_endian
                                                    character_set = 'UTF-8' ).
      cl_abap_unit_assert=>assert_true( boolc( encoding <> lo_encoding ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD two_ixml_instances.
    DATA lo_ixml TYPE REF TO zif_excel_ixml.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      CASE ixml_or_isxml.
        WHEN ixml.
          lo_ixml = lth_wrap_ixml=>create( ).
        WHEN isxml.
          lo_ixml = zcl_excel_ixml=>create( ).
      ENDCASE.
      cl_abap_unit_assert=>assert_equals( act = lo_ixml
                                          exp = ixml_or_isxml ).
    ENDLOOP.
  ENDMETHOD.

  METHOD two_ixml_stream_factories.
    DATA lo_streamfactory TYPE REF TO zif_excel_ixml_stream_factory.

    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      stream_factory = ixml_or_isxml->create_stream_factory( ).
      lo_streamfactory = ixml_or_isxml->create_stream_factory( ).
      cl_abap_unit_assert=>assert_true( boolc( lo_streamfactory <> stream_factory ) ).
    ENDLOOP.
  ENDMETHOD.
*  METHOD two_parsers.
*    DATA lo_istream_2 TYPE REF TO zif_excel_ixml_istream.
*    DATA lo_parser_2  TYPE REF TO zif_excel_ixml_parser.
*
*    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
**         WHERE table_line = isxml
*         .
*      document = ixml_or_isxml->create_document( ).
*      stream_factory = ixml_or_isxml->create_stream_factory( ).
*
*      xstring = cl_abap_codepage=>convert_to( |<B/>| ).
*      istream = stream_factory->create_istream_xstring( xstring ).
*      parser = ixml_or_isxml->create_parser( stream_factory = stream_factory
*                                             istream        = istream
*                                             document       = document ).
*      parser->set_normalizing( abap_true ).
*      parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
*      rc = parser->parse( ).
*
*      string = '<A/>'.
*      lo_istream_2 = stream_factory->create_istream_string( string ).
*      parser = ixml_or_isxml->create_parser( stream_factory = stream_factory
*                                             istream        = lo_istream_2
*                                             document       = document ).
*      rc = parser->parse( ).
*
*      element = document->get_root_element( ).
*      string = element->get_name( ).
*
*      " The second parsing is ignored
*      cl_abap_unit_assert=>assert_equals( act = string
*                                          exp = 'B' ).
*    ENDLOOP.
*  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_isxml_render IMPLEMENTATION.
  METHOD most_simple_valid_xml.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml.
      document = ixml_or_isxml->create_document( ).
      element = document->create_simple_element( name   = 'ROOT'
                                                 parent = document ).
      stream_factory = ixml_or_isxml->create_stream_factory( ).
      GET REFERENCE OF xstring INTO ref_xstring.
      ostream = stream_factory->create_ostream_xstring( ref_xstring ).
      renderer = ixml_or_isxml->create_renderer( ostream  = ostream
                                                 document = document ).
      CLEAR xstring.
      rc = renderer->render( ).
      string = cl_abap_codepage=>convert_from( xstring ).
      cl_abap_unit_assert=>assert_equals( act = string
                                          exp = '<?xml version="1.0"?><ROOT/>' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD namespace.
    LOOP AT ixml_and_isxml INTO ixml_or_isxml
*         WHERE table_line = ixml
         WHERE table_line = isxml.
      document = lth_ixml_isxml=>parse(
          ixml_or_isxml = ixml_or_isxml
          xml_string    = `<nsprefix:A xmlns="dnsuri" xmlns:nsprefix="nsuri" nsprefix:attr="1" attr="2"><B attr="3"/></nsprefix:A>` ).
      string = lth_ixml_isxml=>render( ixml_or_isxml ).
      cl_abap_unit_assert=>assert_equals(
          act = string
          exp = `<nsprefix:A xmlns:nsprefix="nsuri" xmlns="dnsuri" nsprefix:attr="1" attr="2"><B attr="3"/></nsprefix:A>` ).
      ASSERT 1 = 1. " debug helper
    ENDLOOP.
  ENDMETHOD.

  METHOD setup.
    ixml_and_isxml = get_ixml_and_isxml( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_parser IMPLEMENTATION.
  METHOD empty_xml.
    ixml = cl_ixml=>create( ).
    stream_factory = ixml->create_stream_factory( ).
    CLEAR xstring.
    istream = stream_factory->create_istream_xstring( xstring ).
    document = ixml->create_document( ).
    parser = ixml->create_parser( stream_factory = stream_factory
                                  istream        = istream
                                  document       = document ).
    rc = parser->parse( ).
    cl_abap_unit_assert=>assert_equals( act = rc
                                        exp = lcl_isxml=>ixml_mr-parser_error ).
    num_errors = parser->num_errors( ).
    cl_abap_unit_assert=>assert_equals( act = num_errors
                                        exp = 1 ).
    error = parser->get_error( index = 0 ).
    reason = error->get_reason( ).
    cl_abap_unit_assert=>assert_equals( act = reason
                                        exp = `unexpected end-of-file` ).
  ENDMETHOD.

  METHOD end_tag_doesnt_match_begin_tag.
    ixml = cl_ixml=>create( ).
    xstring = cl_abap_codepage=>convert_to( |<A></B>| ).
    stream_factory = ixml->create_stream_factory( ).
    istream = stream_factory->create_istream_xstring( xstring ).
    document = ixml->create_document( ).
    parser = ixml->create_parser( stream_factory = stream_factory
                                  istream        = istream
                                  document       = document ).
    rc = parser->parse( ).
    cl_abap_unit_assert=>assert_equals( act = rc
                                        exp = lcl_isxml=>ixml_mr-parser_error ).
    num_errors = parser->num_errors( ).
    cl_abap_unit_assert=>assert_equals( act = num_errors
                                        exp = 1 ).
    error = parser->get_error( index = 0 ).
    reason = error->get_reason( ).
    cl_abap_unit_assert=>assert_equals( act = reason
                                        exp = `end tag 'B' does not match begin tag 'A'` ).
  ENDMETHOD.

  METHOD most_simple_valid_xml.
    ixml = cl_ixml=>create( ).
    stream_factory = ixml->create_stream_factory( ).
    xstring = cl_abap_codepage=>convert_to( |<A/>| ).
    istream = stream_factory->create_istream_xstring( xstring ).
    document = ixml->create_document( ).
    parser = ixml->create_parser( stream_factory = stream_factory
                                  istream        = istream
                                  document       = document ).
    parser->set_normalizing( abap_true ).
    parser->set_validating( mode = zif_excel_ixml_parser=>co_no_validation ).
    rc = parser->parse( ).
    cl_abap_unit_assert=>assert_equals( act = rc
                                        exp = lcl_isxml=>ixml_mr-dom_ok ).
    num_errors = parser->num_errors( ).
    cl_abap_unit_assert=>assert_equals( act = num_errors
                                        exp = 0 ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_rewrite_xml_via_sxml IMPLEMENTATION.
  METHOD default_namespace.
    string = rewrite_xml_via_sxml(
                 `<A xmlns="dnsuri" xmlns:nsprefix="nsuri" nsprefix:attr="1" attr="2"><B attr="3"/></A>` ).
    cl_abap_unit_assert=>assert_equals(
        act = string
        exp = `<A nsprefix:attr="1" attr="2" xmlns="dnsuri" xmlns:nsprefix="nsuri"><B attr="3"/></A>` ).

    set_current_parsed_element( iv_index = 1 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'A'
                                                                    iv_namespace = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'nsprefix'
                                                                      iv_nsuri  = 'nsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 2 )
                                        exp = get_expected_nsbinding( iv_nsuri = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_attribute( iv_index = 1 )
                                        exp = get_expected_attribute( iv_name      = 'attr'
                                                                      iv_namespace = 'nsuri'
                                                                      iv_prefix    = 'nsprefix' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_attribute( iv_index = 2 )
                                        exp = get_expected_attribute( iv_name = 'attr' ) ).
    set_current_parsed_element( iv_index = 2 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'B'
                                                                    iv_namespace = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'nsprefix'
                                                                      iv_nsuri  = 'nsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 2 )
                                        exp = get_expected_nsbinding( iv_nsuri = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_attribute( iv_index = 1 )
                                        exp = get_expected_attribute( iv_name = 'attr' ) ).
  ENDMETHOD.

  METHOD default_namespace_removed.
    string = rewrite_xml_via_sxml( `<A><B xmlns="dnsuri"><C xmlns=""><D/></C></B></A>` ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = `<A><B xmlns="dnsuri"><C xmlns=""><D/></C></B></A>` ).

    set_current_parsed_element( iv_index = 1 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name = 'A' ) ).
    set_current_parsed_element( iv_index = 2 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'B'
                                                                    iv_namespace = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_nsuri = 'dnsuri' ) ).
    set_current_parsed_element( iv_index = 3 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name = 'C' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_nsuri = '' ) ).
    set_current_parsed_element( iv_index = 4 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name = 'D' ) ).
  ENDMETHOD.

  METHOD namespace.
    string = rewrite_xml_via_sxml(
        `<nsprefix:A xmlns="dnsuri" xmlns:nsprefix="nsuri" nsprefix:attr="1" attr="2"><B attr="3"/></nsprefix:A>` ).
    cl_abap_unit_assert=>assert_equals(
        act = string
        exp = `<nsprefix:A nsprefix:attr="1" attr="2" xmlns="dnsuri" xmlns:nsprefix="nsuri"><B attr="3"/></nsprefix:A>` ).

    cl_abap_unit_assert=>assert_equals( act = rewrite_xml_via_sxml( `<A xmlns=""/>` )
                                        exp = `<A xmlns=""/>` ).

    cl_abap_unit_assert=>assert_equals( act = rewrite_xml_via_sxml( `<A xmlns=""><B/></A>` )
                                        exp = `<A xmlns=""><B/></A>` ).

    cl_abap_unit_assert=>assert_equals( act = rewrite_xml_via_sxml( `<A><B xmlns=""/></A>` )
                                        exp = `<A><B xmlns=""/></A>` ).
  ENDMETHOD.

  METHOD namespace_2.
    string = rewrite_xml_via_sxml( `<A xmlns="dnsuri" xmlns:nsprefix="nsuri"><B/><nsprefix:C/></A>` ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = `<A xmlns="dnsuri" xmlns:nsprefix="nsuri"><B/><nsprefix:C/></A>` ).

    set_current_parsed_element( iv_index = 1 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'A'
                                                                    iv_namespace = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'nsprefix'
                                                                      iv_nsuri  = 'nsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 2 )
                                        exp = get_expected_nsbinding( iv_nsuri = 'dnsuri' ) ).
    set_current_parsed_element( iv_index = 2 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'B'
                                                                    iv_namespace = 'dnsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'nsprefix'
                                                                      iv_nsuri  = 'nsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 2 )
                                        exp = get_expected_nsbinding( iv_nsuri = 'dnsuri' ) ).
    set_current_parsed_element( iv_index = 3 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'C'
                                                                    iv_namespace = 'nsuri'
                                                                    iv_prefix    = 'nsprefix' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'nsprefix'
                                                                      iv_nsuri  = 'nsuri' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 2 )
                                        exp = get_expected_nsbinding( iv_nsuri = 'dnsuri' ) ).
  ENDMETHOD.

  METHOD namespace_3.
    string = rewrite_xml_via_sxml( `<a:A xmlns:a="nsuri_a"><b:B xmlns:b="nsuri_b"/></a:A>` ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = `<a:A xmlns:a="nsuri_a"><b:B xmlns:b="nsuri_b"/></a:A>` ).

    set_current_parsed_element( iv_index = 1 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'A'
                                                                    iv_namespace = 'nsuri_a'
                                                                    iv_prefix    = 'a' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'a'
                                                                      iv_nsuri  = 'nsuri_a' ) ).
    set_current_parsed_element( iv_index = 2 ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element( )
                                        exp = get_expected_element( iv_name      = 'B'
                                                                    iv_namespace = 'nsuri_b'
                                                                    iv_prefix    = 'b' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 1 )
                                        exp = get_expected_nsbinding( iv_prefix = 'b'
                                                                      iv_nsuri  = 'nsuri_b' ) ).
    cl_abap_unit_assert=>assert_equals( act = get_parsed_element_nsbinding( iv_index = 2 )
                                        exp = get_expected_nsbinding( iv_prefix = 'a'
                                                                      iv_nsuri  = 'nsuri_a' ) ).
  ENDMETHOD.

  METHOD rewrite_xml_via_sxml.
    rv_string = lcl_rewrite_xml_via_sxml=>execute( iv_xml_string = iv_xml_string
                                                   iv_trace      = abap_true ).
  ENDMETHOD.
*  METHOD parse_render.
*    TYPES:
*      BEGIN OF ts_level,
*        number     TYPE i,
*        nsbindings TYPE if_sxml_named=>nsbindings,
*      END OF ts_level.
*
*    DATA lv_current_level    TYPE i.
*    DATA ls_level            TYPE ts_level.
*    DATA lt_level            TYPE STANDARD TABLE OF ts_level WITH DEFAULT KEY.
*    DATA lo_reader           TYPE REF TO if_sxml_reader.
*    DATA string_writer       TYPE REF TO cl_sxml_string_writer.
*    DATA writer              TYPE REF TO if_sxml_writer.
*    DATA node                TYPE REF TO if_sxml_node.
*    DATA close_element       TYPE REF TO if_sxml_close_element.
*    DATA open_element        TYPE REF TO if_sxml_open_element.
*    DATA lt_nsbinding        TYPE if_sxml_named=>nsbindings.
*    DATA lt_attribute        TYPE if_sxml_attribute=>attributes.
*    DATA ls_complete_element TYPE ts_complete_element.
*    DATA lr_nsbinding        TYPE REF TO if_sxml_named=>nsbinding.
*    DATA ls_nsbinding        TYPE ts_nsbinding.
*    DATA lo_attribute        TYPE REF TO if_sxml_attribute.
*    DATA ls_attribute        TYPE ts_attribute.
*    DATA value_node          TYPE REF TO if_sxml_value_node.
*
*    FIELD-SYMBOLS <ls_level> TYPE ts_level.
*
*    lv_current_level = 1.
*    ls_level-number = lv_current_level.
*    INSERT ls_level INTO TABLE lt_level ASSIGNING <ls_level>.
*
*    lo_reader = cl_sxml_string_reader=>create( input = cl_abap_codepage=>convert_to( iv_xml_string ) ).
*    string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_xml10 ).
*    writer = string_writer.
*
*    DO.
*      node = lo_reader->read_next_node( ).
*      IF node IS NOT BOUND.
*        " End of XML
*        EXIT.
*      ENDIF.
*
*      CASE node->type.
*        WHEN node->co_nt_attribute.
*          "should not happen in OO parsing (READ_NEXT_NODE)
*          RAISE EXCEPTION TYPE lcx_unexpected.
*
*        WHEN node->co_nt_element_close.
*
*          DELETE lt_level INDEX lv_current_level.
*          lv_current_level = lv_current_level - 1.
*          READ TABLE lt_level INDEX lv_current_level ASSIGNING <ls_level>.
*
*          close_element = writer->new_close_element( ).
*          writer->write_node( close_element ).
**          writer->write_node( node ).
*
*        WHEN node->co_nt_element_open.
*          open_element ?= node.
*
*          lt_nsbinding = lo_reader->get_nsbindings( ).
*          lt_attribute = open_element->get_attributes( ).
*
*          CLEAR ls_complete_element.
*          ls_complete_element-element-name      = open_element->qname-name.
*          ls_complete_element-element-namespace = open_element->qname-namespace.
*          ls_complete_element-element-prefix    = open_element->prefix.
*          LOOP AT lt_nsbinding REFERENCE INTO lr_nsbinding.
*            ls_nsbinding-prefix = lr_nsbinding->prefix.
*            ls_nsbinding-nsuri  = lr_nsbinding->nsuri.
*            INSERT ls_nsbinding INTO TABLE ls_complete_element-nsbindings.
*          ENDLOOP.
*          LOOP AT lt_attribute INTO lo_attribute.
*            ls_attribute-name      = lo_attribute->qname-name.
*            ls_attribute-namespace = lo_attribute->qname-namespace.
*            ls_attribute-prefix    = lo_attribute->prefix.
*            INSERT ls_attribute INTO TABLE ls_complete_element-attributes.
*          ENDLOOP.
*          INSERT ls_complete_element INTO TABLE complete_parsed_elements.
*
*          IF open_element->prefix IS INITIAL.
*            open_element = writer->new_open_element( name = open_element->qname-name ).
*          ELSE.
*            open_element = writer->new_open_element( name   = open_element->qname-name
*                                                     nsuri  = open_element->qname-namespace
*                                                     prefix = open_element->prefix ).
*          ENDIF.
*
*          open_element->set_attributes( lt_attribute ).
*
*          LOOP AT lt_nsbinding REFERENCE INTO lr_nsbinding.
*            READ TABLE <ls_level>-nsbindings TRANSPORTING NO FIELDS WITH KEY prefix = lr_nsbinding->prefix
*                                                                             nsuri  = lr_nsbinding->nsuri.
*            IF sy-subrc <> 0.
*              " It's the first time the default namespace is used,
*              " or if it has been changed, then declare it.
*              IF lr_nsbinding->prefix IS INITIAL.
*                open_element->set_attribute( name  = 'xmlns'
*                                             value = lr_nsbinding->nsuri ).
*              ELSE.
*                writer->write_namespace_declaration( nsuri  = lr_nsbinding->nsuri
*                                                     prefix = lr_nsbinding->prefix ).
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
*
**          " Default namespace declaration is set (xmlns="...")
**          READ TABLE lt_nsbinding REFERENCE INTO lr_nsbinding WITH TABLE KEY prefix = ``.
**          IF sy-subrc = 0.
**
***            IF open_element->prefix IS INITIAL.
**            IF open_element->qname-namespace = lr_nsbinding->nsuri.
**              " Element URI = default namespace URI
**              open_element = writer->new_open_element( name = open_element->qname-name ).
**            ELSE.
**              " Element URI <> default namespace URI.
**              open_element = writer->new_open_element( name   = open_element->qname-name
**                                                       nsuri  = open_element->qname-namespace
**                                                       prefix = open_element->prefix ).
**            ENDIF.
**
**            open_element->set_attributes( lt_attribute ).
**
**            READ TABLE <ls_level>-nsbindings TRANSPORTING NO FIELDS WITH KEY prefix = ``
**                                                                             nsuri  = lr_nsbinding->nsuri.
**            IF sy-subrc <> 0.
**              " It's the first time the default namespace is used,
**              " or if it has been changed, then declare it.
**              open_element->set_attribute( name  = 'xmlns'
**                                           value = lr_nsbinding->nsuri ).
**            ENDIF.
**
**          ELSE.
***          READ TABLE lt_nsbinding REFERENCE INTO lr_nsbinding WITH TABLE KEY prefix = ``.
**            IF open_element->qname-namespace IS NOT INITIAL.
**              READ TABLE lt_nsbinding REFERENCE INTO lr_nsbinding WITH KEY prefix = open_element->prefix
**                                                                           nsuri  = open_element->qname-namespace.
**              IF sy-subrc = 0.
**                open_element = writer->new_open_element( name   = open_element->qname-name
**                                                         nsuri  = open_element->qname-namespace
**                                                         prefix = open_element->prefix ).
**              ENDIF.
**            ENDIF.
**
***            lv_add_default_namespace = abap_false.
***            IF open_element->qname-namespace <> lr_nsbinding->nsuri.
***              lv_add_default_namespace = abap_true.
***            ELSE.
***              READ TABLE <ls_level>-nsbindings TRANSPORTING NO FIELDS WITH TABLE KEY prefix = ``.
***              IF sy-subrc <> 0.
***                lv_add_default_namespace = abap_true.
***              ENDIF.
***            ENDIF.
***            IF lv_add_default_namespace = abap_true.
***              open_element->set_attribute( name  = 'xmlns'
***                                           value = lr_nsbinding->nsuri ).
***            ENDIF.
**          ENDIF.
*
*          lv_current_level = lv_current_level + 1.
*          ls_level-number     = lv_current_level.
*          ls_level-nsbindings = lt_nsbinding.
*          INSERT ls_level INTO TABLE lt_level ASSIGNING <ls_level>.
*
*          writer->write_node( node = open_element ).
*
*        WHEN node->co_nt_final.
*          "should not happen in OO parsing
*          RAISE EXCEPTION TYPE lcx_unexpected.
*
*        WHEN node->co_nt_initial.
*          "should not happen in OO parsing
*          RAISE EXCEPTION TYPE lcx_unexpected.
*
*        WHEN node->co_nt_value.
*
*          value_node = writer->new_value( ).
*          writer->write_node( value_node ).
**          writer->write_node( node ).
*
*        WHEN OTHERS.
*          "should not happen whatever it's OO or token parsing
*          RAISE EXCEPTION TYPE lcx_unexpected.
*      ENDCASE.
*    ENDDO.
*    rv_string = cl_abap_codepage=>convert_from( string_writer->get_output( ) ).
*  ENDMETHOD.

  METHOD get_expected_attribute.
    rs_result-name      = iv_name.
    rs_result-namespace = iv_namespace.
    rs_result-prefix    = iv_prefix.
  ENDMETHOD.

  METHOD get_expected_element.
    rs_result-name      = iv_name.
    rs_result-namespace = iv_namespace.
    rs_result-prefix    = iv_prefix.
  ENDMETHOD.

  METHOD get_expected_nsbinding.
    rs_result-prefix = iv_prefix.
    rs_result-nsuri  = iv_nsuri.
  ENDMETHOD.

  METHOD get_parsed_element.
    DATA lr_complete_parsed_element TYPE REF TO lcl_rewrite_xml_via_sxml=>ts_complete_element.

    READ TABLE lcl_rewrite_xml_via_sxml=>complete_parsed_elements REFERENCE INTO lr_complete_parsed_element INDEX parsed_element_index.
    IF sy-subrc = 0.
      rs_result = lr_complete_parsed_element->element.
    ENDIF.
  ENDMETHOD.

  METHOD get_parsed_element_attribute.
    DATA lr_complete_parsed_element TYPE REF TO lcl_rewrite_xml_via_sxml=>ts_complete_element.

    READ TABLE lcl_rewrite_xml_via_sxml=>complete_parsed_elements REFERENCE INTO lr_complete_parsed_element INDEX parsed_element_index.
    IF sy-subrc = 0.
      READ TABLE lr_complete_parsed_element->attributes INTO rs_result INDEX iv_index.
    ENDIF.
  ENDMETHOD.

  METHOD get_parsed_element_nsbinding.
    DATA lr_complete_parsed_element TYPE REF TO lcl_rewrite_xml_via_sxml=>ts_complete_element.

    READ TABLE lcl_rewrite_xml_via_sxml=>complete_parsed_elements REFERENCE INTO lr_complete_parsed_element INDEX parsed_element_index.
    IF sy-subrc = 0.
      READ TABLE lr_complete_parsed_element->nsbindings INTO rs_result INDEX iv_index.
    ENDIF.
  ENDMETHOD.

  METHOD set_current_parsed_element.
    parsed_element_index = iv_index.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_sxml_reader IMPLEMENTATION.
  METHOD empty_object_oriented_parsing.
    xstring = cl_abap_codepage=>convert_to( '<ROOTNODE/>' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_element_open ).
    node_open ?= node.
    cl_abap_unit_assert=>assert_equals( act = node_open->qname-name
                                        exp = 'ROOTNODE' ).
    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_element_close ).
    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_not_bound( node ).
  ENDMETHOD.

  METHOD empty_token_based_parsing.
    xstring = cl_abap_codepage=>convert_to( '<ROOTNODE/>' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_initial ).
    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_open ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOTNODE' ).
    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_close ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOTNODE' ).
    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_final ).
  ENDMETHOD.

  METHOD empty_xml.
    DATA parse_error TYPE REF TO cx_sxml_parse_error.

    CLEAR xstring.
    reader = cl_sxml_string_reader=>create( xstring ).
    TRY.
        node = reader->read_next_node( ).
        cl_abap_unit_assert=>fail( msg = 'should have failed' ).
      CATCH cx_root INTO error.
        error_rtti = cl_abap_typedescr=>describe_by_object_ref( error ).
        cl_abap_unit_assert=>assert_equals( act = error_rtti->get_relative_name( )
                                            exp = 'CX_SXML_PARSE_ERROR' ).
        parse_error ?= error.
        cl_abap_unit_assert=>assert_equals( act = parse_error->textid
                                            exp = parse_error->kernel_parser ).
        cl_abap_unit_assert=>assert_equals( act = parse_error->error_text
                                            exp = 'BOM / charset detection failed' ).
    ENDTRY.
  ENDMETHOD.

  METHOD invalid_xml.
    xstring = cl_abap_codepage=>convert_to( '<' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_not_bound( node ).
  ENDMETHOD.

  METHOD invalid_xml_eof_reached.
    xstring = cl_abap_codepage=>convert_to( '<A>' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    node = reader->read_next_node( ).
    TRY.
        node = reader->read_next_node( ).
        cl_abap_unit_assert=>assert_not_bound( node ).
        cl_abap_unit_assert=>fail( msg = 'should have failed' ).
      CATCH cx_root INTO error.
        error_rtti = cl_abap_typedescr=>describe_by_object_ref( error ).
        cl_abap_unit_assert=>assert_equals( act = error_rtti->get_relative_name( )
                                            exp = 'CX_SXML_PARSE_ERROR' ).
        parse_error ?= error.
        cl_abap_unit_assert=>assert_equals( act = parse_error->textid
                                            exp = parse_error->kernel_parser ).
        cl_abap_unit_assert=>assert_equals( act = parse_error->error_text
                                            exp = '<EOF> reached' ).
    ENDTRY.
  ENDMETHOD.

  METHOD invalid_xml_not_wellformed.
    xstring = cl_abap_codepage=>convert_to( '<A></B>' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    node = reader->read_next_node( ).
    TRY.
        node = reader->read_next_node( ).
        cl_abap_unit_assert=>assert_not_bound( node ).
        cl_abap_unit_assert=>fail( msg = 'should have failed' ).
      CATCH cx_root INTO error.
        error_rtti = cl_abap_typedescr=>describe_by_object_ref( error ).
        cl_abap_unit_assert=>assert_equals( act = error_rtti->get_relative_name( )
                                            exp = 'CX_SXML_PARSE_ERROR' ).
        parse_error ?= error.
        cl_abap_unit_assert=>assert_equals( act = parse_error->textid
                                            exp = parse_error->kernel_parser ).
        cl_abap_unit_assert=>assert_equals( act = parse_error->error_text
                                            exp = 'document not wellformed' ).
    ENDTRY.
  ENDMETHOD.

  METHOD object_oriented_parsing.
    xstring = cl_abap_codepage=>convert_to( '<ROOTNODE ATTR="Efe=">Efe=</ROOTNODE>' ).
    reader = cl_sxml_string_reader=>create( xstring ).

    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_element_open ).
    node_open ?= node.
    cl_abap_unit_assert=>assert_equals( act = node_open->qname-name
                                        exp = 'ROOTNODE' ).

    node_attr = node_open->get_attribute_value( 'ATTR' ).
    cl_abap_unit_assert=>assert_bound( node_attr ).
    cl_abap_unit_assert=>assert_equals( act = node_attr->type
                                        exp = node_attr->co_vt_text ).
    cl_abap_unit_assert=>assert_equals( act = node_attr->get_value( )
                                        exp = 'Efe=' ).
    xstring = 'E0'.
    cl_abap_unit_assert=>assert_equals( act = node_attr->get_value_raw( )
                                        exp = xstring ).

    node = reader->read_current_node( ).

    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_value ).
    value_node ?= node.
    cl_abap_unit_assert=>assert_equals( act = value_node->get_value( )
                                        exp = 'Efe=' ).
    xstring = 'E0'.
    cl_abap_unit_assert=>assert_equals( act = value_node->get_value_raw( )
                                        exp = xstring ).
  ENDMETHOD.

  METHOD token_based_parsing.
    xstring = cl_abap_codepage=>convert_to( '<ROOTNODE ATTR="UFE=">UFE=</ROOTNODE>' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_initial ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_open ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOTNODE' ).

    reader->next_attribute( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_attribute ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ATTR' ).
    cl_abap_unit_assert=>assert_equals( act = reader->value_type
                                        exp = if_sxml_value=>co_vt_text ).
    cl_abap_unit_assert=>assert_equals( act = reader->value
                                        exp = 'UFE=' ).
    CLEAR xstring.
    cl_abap_unit_assert=>assert_equals( act = reader->value_raw
                                        exp = xstring ).

    reader->next_attribute( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_open ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOTNODE' ).
    reader->current_node( ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_value ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOTNODE' ).
    cl_abap_unit_assert=>assert_equals( act = reader->value_type
                                        exp = if_sxml_value=>co_vt_text ).
    cl_abap_unit_assert=>assert_equals( act = reader->value
                                        exp = 'UFE=' ).
    CLEAR xstring.
    cl_abap_unit_assert=>assert_equals( act = reader->value_raw
                                        exp = xstring ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_close ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOTNODE' ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_final ).
  ENDMETHOD.

  METHOD xml_header_is_ignored.
    xstring = cl_abap_codepage=>convert_to( '<?xml version="1.0" encoding="utf-8" standalone="yes"?><ROOT/>' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_initial ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_open ).
    cl_abap_unit_assert=>assert_equals( act = reader->name
                                        exp = 'ROOT' ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_close ).

    reader->next_node( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_final ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_sxml_writer IMPLEMENTATION.
  METHOD attribute_namespace.
    open_element = writer->new_open_element( name = 'A' ).
    open_element->set_attribute( name   = 'attr'
                                 nsuri  = 'http://...'
                                 prefix = 'a'
                                 value  = '1' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A a:attr="1" xmlns:a="http://..."/>' ).
  ENDMETHOD.

  METHOD attribute_xml_namespace.
    open_element = writer->new_open_element( name = 'A' ).
    open_element->set_attribute( name   = 'space'
                                 nsuri  = 'http://www.w3.org/XML/1998/namespace'
                                 prefix = 'xml'
                                 value  = 'preserve' ).
    writer->write_node( open_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xml:space="preserve"/>' ).
  ENDMETHOD.

  METHOD get_output.
    DATA lo_string_writer TYPE REF TO cl_sxml_string_writer.
    DATA lv_xstring       TYPE xstring.

    lo_string_writer ?= io_writer.
    lv_xstring = lo_string_writer->get_output( ).
    rv_result = cl_abap_codepage=>convert_from( lv_xstring ).
  ENDMETHOD.

  METHOD most_simple_valid_xml.
    open_element = writer->new_open_element( 'A' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A/>' ).
  ENDMETHOD.

  METHOD namespace.
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'http://...'
                                             prefix = 'a' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<a:A xmlns:a="http://..."/>' ).
  ENDMETHOD.

  METHOD namespace_default.
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'http://...'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_node( open_element ).
    open_element = writer->new_open_element( name   = 'B'
                                             nsuri  = 'http://...'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xmlns="http://..."><B/></A>' ).
  ENDMETHOD.

  METHOD namespace_default_by_attribute.
    open_element = writer->new_open_element( name = 'A' ).
    open_element->set_attribute( name  = 'xmlns'
                                 value = 'http://...' ).
    writer->write_node( open_element ).
    open_element = writer->new_open_element( name = 'B' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xmlns="http://..."><B/></A>' ).
  ENDMETHOD.

  METHOD namespace_inheritance.
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'http://...'
                                             prefix = 'a' ).
    writer->write_node( open_element ).
    open_element = writer->new_open_element( name   = 'B'
                                             nsuri  = 'http://...'
                                             prefix = 'a' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<a:A xmlns:a="http://..."><a:B/></a:A>' ).
  ENDMETHOD.

  METHOD namespace_set_prefix.
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'http://...'
                                             prefix = 'a' ).
    open_element->set_prefix( 'b' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<b:A xmlns:b="http://..."/>' ).
  ENDMETHOD.

  METHOD object_oriented_rendering.
    " WRITE_NODE and NEW_* methods
    " NB: NEW_* methods are static methods (i.e. it's valid: DATA(open_element) = cl_sxml_writer=>if_sxml_writer~new_open_element( 'ROOTNODE' ).)
    open_element = writer->new_open_element( 'ROOTNODE' ).
    open_element->set_attribute( name  = 'ATTR'
                                 value = '5' ).
    writer->write_node( open_element ).
    " WRITE_ATTRIBUTE and WRITE_ATTRIBUTE_RAW can also be used, but only after the WRITE_NODE of an element opening tag
    writer->write_attribute( name  = 'ATTR2'
                             value = 'A' ).
    " WRITE_ATTRIBUTE_RAW writes in Base64
    writer->write_attribute_raw( name  = 'ATTR3'
                                 value = '5051' ).
    value_node = writer->new_value( ).
    value_node->set_value( 'HELLO' ).
    writer->write_node( value_node ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<ROOTNODE ATTR="5" ATTR2="A" ATTR3="UFE=">HELLO</ROOTNODE>' ).
  ENDMETHOD.

  METHOD setup.
    writer = cl_sxml_string_writer=>create( ).
  ENDMETHOD.

  METHOD token_based_rendering.
    writer->open_element( 'ROOTNODE' ).
    " WRITE_ATTRIBUTE and WRITE_ATTRIBUTE_RAW can be used only after OPEN_ELEMENT
    writer->write_attribute( name  = 'ATTR'
                             value = '5' ).
    writer->write_attribute_raw( name  = 'ATTR2'
                                 value = '5051' ).
    writer->write_value( 'HELLO' ).
    writer->open_element( 'NODE' ).
    writer->write_value_raw( '5051' ).
    writer->close_element( ).
    writer->close_element( ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<ROOTNODE ATTR="5" ATTR2="UFE=">HELLO<NODE>UFE=</NODE></ROOTNODE>' ).
  ENDMETHOD.

  METHOD write_namespace_declaration.
    open_element = writer->new_open_element( name = 'A' ).
    writer->write_namespace_declaration( nsuri  = 'nsuri'
                                         prefix = 'nsprefix' ).
    writer->write_node( open_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xmlns:nsprefix="nsuri"/>' ).
  ENDMETHOD.

  METHOD write_namespace_declaration_2.
    " <A xmlns:nsprefix="nsuri"/>
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'dnsuri'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_1'
                                         prefix = 'nsprefix_1' ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_2'
                                         prefix = 'nsprefix_2' ).
    writer->write_node( open_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals(
        act = string
        exp = '<A xmlns:nsprefix_1="nsuri_1" xmlns:nsprefix_2="nsuri_2" xmlns="dnsuri"/>' ).
  ENDMETHOD.

  METHOD write_namespace_declaration_3.
    " <A xmlns:nsprefix="nsuri"/>
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'dnsuri'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_2'
                                         prefix = 'nsprefix_2' ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_1'
                                         prefix = 'nsprefix_1' ).
    writer->write_node( open_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals(
        act = string
        exp = '<A xmlns:nsprefix_2="nsuri_2" xmlns:nsprefix_1="nsuri_1" xmlns="dnsuri"/>' ).
  ENDMETHOD.

  METHOD write_namespace_declaration_4.
    " <A xmlns:nsprefix="nsuri"/>
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'dnsuri'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_1'
                                         prefix = 'nsprefix_1' ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_2'
                                         prefix = 'nsprefix_2' ).
    open_element->set_attribute( name  = 'a'
                                 value = '1' ).
*    open_element->set_attribute( name = 'a' nsuri = 'nsuri_1' prefix = 'nsprefix_1' value = '1' ).
    writer->write_node( open_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals(
        act = string
        exp = '<A a="1" xmlns:nsprefix_1="nsuri_1" xmlns:nsprefix_2="nsuri_2" xmlns="dnsuri"/>' ).
*                                        exp = '<A nsprefix_1:a="1" xmlns:nsprefix_1="nsuri_1" xmlns:nsprefix_2="nsuri_2" xmlns="dnsuri"/>' ).
  ENDMETHOD.

  METHOD write_namespace_declaration_5.
    " <A xmlns:nsprefix="nsuri"/>
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'dnsuri'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    open_element->set_attribute( name  = 'a'
                                 value = '1' ).
*    open_element->set_attribute( name = 'a' nsuri = 'nsuri_1' prefix = 'nsprefix_1' value = '1' ).
    writer->write_node( open_element ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_1'
                                         prefix = 'nsprefix_1' ).
    writer->write_namespace_declaration( nsuri  = 'nsuri_2'
                                         prefix = 'nsprefix_2' ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals(
        act = string
*        exp = '<A nsprefix_1:a="1" xmlns="dnsuri" xmlns:nsprefix_1="nsuri_1" xmlns:nsprefix_2="nsuri_2"/>' ).
        exp = '<A a="1" xmlns="dnsuri" xmlns:nsprefix_1="nsuri_1" xmlns:nsprefix_2="nsuri_2"/>' ).
  ENDMETHOD.

  METHOD write_namespace_declaration_6.
    " <A xmlns="dnsuri" xmlns:nsprefix="nsuri"><B/><nsprefix:C/></A>
    open_element = writer->new_open_element( name   = 'A'
                                             nsuri  = 'dnsuri'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_namespace_declaration( nsuri  = 'nsuri'
                                         prefix = 'nsprefix' ).
    writer->write_node( open_element ).

    open_element = writer->new_open_element( name   = 'B'
                                             nsuri  = 'dnsuri'
                                             prefix = if_sxml_named=>co_use_default_xmlns ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    open_element = writer->new_open_element( name   = 'C'
                                             nsuri  = 'nsuri'
                                             prefix = 'nsprefix' ).
    writer->write_node( open_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string = get_output( writer ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A xmlns:nsprefix="nsuri" xmlns="dnsuri"><B/><nsprefix:C/></A>' ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_ixml IMPLEMENTATION.
  METHOD create_document.
    ixml = cl_ixml=>create( ).
    document = ixml->create_document( ).
  ENDMETHOD.

  METHOD parse.
    DATA lv_xstring TYPE xstring.
    DATA lo_istream TYPE REF TO if_ixml_istream.
    DATA lo_parser  TYPE REF TO if_ixml_parser.

    " code inspired from the method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007.
    ixml = cl_ixml=>create( ).
    document = ixml->create_document( ).
    encoding = ixml->create_encoding( byte_order    = if_ixml_encoding=>co_none "co_platform_endian
                                      character_set = 'UTF-16' ).
    document->set_encoding( encoding ).
    stream_factory = ixml->create_stream_factory( ).

    lv_xstring = cl_abap_codepage=>convert_to( xml_string ).
    lo_istream = stream_factory->create_istream_xstring( lv_xstring ).
    lo_parser = ixml->create_parser( stream_factory = stream_factory
                                     istream        = lo_istream
                                     document       = document ).
    lo_parser->set_normalizing( abap_true ).
    lo_parser->set_validating( mode = zif_excel_ixml_parser=>co_no_validation ).
    lo_parser->parse( ).

    ro_result = document.
  ENDMETHOD.

  METHOD render.
    DATA lo_ostream  TYPE REF TO if_ixml_ostream.
    DATA lo_renderer TYPE REF TO if_ixml_renderer.

    stream_factory = ixml->create_stream_factory( ).
    lo_ostream = stream_factory->create_ostream_cstring( rv_result ).
    lo_renderer = ixml->create_renderer( ostream  = lo_ostream
                                         document = document ).
    document->set_declaration( abap_false ).
    " Fills RV_RESULT
    lo_renderer->render( ).
    " remove the UTF-16 BOM (i.e. remove the first character)
    SHIFT rv_result LEFT BY 1 PLACES.
  ENDMETHOD.
ENDCLASS.


CLASS lth_ixml_isxml IMPLEMENTATION.
  METHOD create_document.
    ixml = zcl_excel_ixml=>create( ).
    document = ixml->create_document( ).
  ENDMETHOD.

  METHOD parse.
    DATA lv_xstring TYPE xstring.
    DATA lo_istream TYPE REF TO zif_excel_ixml_istream.
    DATA lo_parser  TYPE REF TO zif_excel_ixml_parser.

    " code inspired from the method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007.
    document = ixml_or_isxml->create_document( ).
    stream_factory = ixml_or_isxml->create_stream_factory( ).

    lv_xstring = cl_abap_codepage=>convert_to( xml_string ).
    lo_istream = stream_factory->create_istream_xstring( lv_xstring ).
    lo_parser = ixml_or_isxml->create_parser( stream_factory = stream_factory
                                              istream        = lo_istream
                                              document       = document ).
    lo_parser->set_normalizing( abap_true ).
    lo_parser->set_validating( mode = zif_excel_ixml_parser=>co_no_validation ).
    lo_parser->parse( ).
    ro_result = document.
  ENDMETHOD.

  METHOD render.
    DATA lr_string   TYPE REF TO string.
    DATA lo_ostream  TYPE REF TO zif_excel_ixml_ostream.
    DATA lo_renderer TYPE REF TO zif_excel_ixml_renderer.

    stream_factory = ixml_or_isxml->create_stream_factory( ).
    GET REFERENCE OF rv_result INTO lr_string.
    lo_ostream = stream_factory->create_ostream_cstring( lr_string ).
    lo_renderer = ixml_or_isxml->create_renderer( ostream  = lo_ostream
                                                  document = document ).
    document->set_declaration( abap_false ).
    " Fills RV_RESULT
    lo_renderer->render( ).
    " remove the UTF-16 BOM (i.e. remove the first character)
    SHIFT rv_result LEFT BY 1 PLACES.
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

  METHOD zif_excel_ixml~create_document.
    DATA lo_ixml_document TYPE REF TO if_ixml_document.

    lo_ixml_document = ixml->create_document( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_document ).
  ENDMETHOD.

  METHOD zif_excel_ixml~create_encoding.
    DATA lo_encoding TYPE REF TO lth_wrap_ixml_encoding.

    CREATE OBJECT lo_encoding.
    lo_encoding->ixml_encoding = ixml->create_encoding( byte_order    = byte_order
                                                        character_set = character_set ).
    rval = lo_encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_parser.
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

  METHOD zif_excel_ixml~create_renderer.
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

  METHOD zif_excel_ixml~create_stream_factory.
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
  METHOD zif_excel_ixml_document~create_element.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->create_element( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element.
    DATA lo_wrap_ixml_parent TYPE REF TO lth_wrap_ixml_node.
    DATA lo_ixml_element     TYPE REF TO if_ixml_element.

    lo_wrap_ixml_parent ?= parent.
    lo_ixml_element = ixml_document->create_simple_element( name   = name
                                                            parent = lo_wrap_ixml_parent->ixml_node ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element_ns.
    DATA lo_wrap_ixml_parent TYPE REF TO lth_wrap_ixml_node.
    DATA lo_ixml_element     TYPE REF TO if_ixml_element.

    lo_wrap_ixml_parent ?= parent.
    lo_ixml_element = ixml_document->create_simple_element_ns( name   = name
                                                               parent = lo_wrap_ixml_parent->ixml_node
                                                               prefix = prefix ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->find_from_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~find_from_name_ns.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->find_from_name_ns( name = name
                                                        uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_elements_by_tag_name.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_document->get_elements_by_tag_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~get_root_element.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_document->get_root_element( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_declaration.
    ixml_document->set_declaration( declaration = declaration ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_encoding.
    DATA wrap_ixml_encoding TYPE REF TO lth_wrap_ixml_encoding.

    wrap_ixml_encoding ?= encoding.
    ixml_document->set_encoding( encoding = wrap_ixml_encoding->ixml_encoding ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_standalone.
    ixml_document->set_standalone( standalone ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_element IMPLEMENTATION.
  METHOD zif_excel_ixml_element~find_from_name.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_element->find_from_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~find_from_name_ns.
    DATA lo_ixml_element TYPE REF TO if_ixml_element.

    lo_ixml_element = ixml_element->find_from_name_ns( depth = depth
                                                       name  = name
                                                       uri   = uri ).
*    IF lo_ixml_element IS BOUND.
*      data(temp_name) = lo_ixml_element->get_name( ).
*      data(temp_nsuri) = lo_ixml_element->get_namespace_uri( ).
*      data(temp_value) = lo_ixml_element->get_value( ).
*    ENDIF.
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_element ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute.
    rval = ixml_element->get_attribute( name = name ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute_node_ns.
    DATA lo_ixml_attribute TYPE REF TO if_ixml_attribute.

    lo_ixml_attribute = ixml_element->get_attribute_node_ns( name = name
                                                             uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_attribute ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_attribute_ns.
    rval = ixml_element->get_attribute_ns( name = name
                                           uri  = uri ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_elements_by_tag_name.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_element->get_elements_by_tag_name( name = name ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~get_elements_by_tag_name_ns.
    DATA lo_ixml_node_collection TYPE REF TO if_ixml_node_collection.

    lo_ixml_node_collection = ixml_element->get_elements_by_tag_name_ns( name = name
                                                                         uri  = uri ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_collection ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~remove_attribute_ns.
    ixml_element->remove_attribute_ns( name = name ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~set_attribute.
    ixml_element->set_attribute( name      = name
                                 namespace = namespace
                                 value     = value ).
  ENDMETHOD.

  METHOD zif_excel_ixml_element~set_attribute_ns.
    ixml_element->set_attribute_ns( name   = name
                                    prefix = prefix
                                    value  = value ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_istream_string IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_istream_xstring IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_named_node_map IMPLEMENTATION.
  METHOD zif_excel_ixml_named_node_map~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_named_node_map->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node IMPLEMENTATION.
  METHOD zif_excel_ixml_node~append_child.
    DATA lo_wrap_ixml_node TYPE REF TO lth_wrap_ixml_node.

    lo_wrap_ixml_node ?= new_child.
    ixml_node->append_child( lo_wrap_ixml_node->ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~clone.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node->clone( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_node->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_attributes.
    DATA lo_ixml_named_node_map TYPE REF TO if_ixml_named_node_map.

    lo_ixml_named_node_map = ixml_node->get_attributes( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_named_node_map ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_children.
    DATA lo_ixml_node_list TYPE REF TO if_ixml_node_list.

    lo_ixml_node_list = ixml_node->get_children( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_list ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_first_child.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node->get_first_child( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_name.
    rval = ixml_node->get_name( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_namespace_prefix.
    rval = ixml_node->get_namespace_prefix( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_namespace_uri.
    rval = ixml_node->get_namespace_uri( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_next.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node->get_next( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_type.
    rval = ixml_node->get_type( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_value.
    rval = ixml_node->get_value( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_namespace_prefix.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_value.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node_collection IMPLEMENTATION.
  METHOD zif_excel_ixml_node_collection~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_node_collection->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.

  METHOD zif_excel_ixml_node_collection~get_length.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node_iterator IMPLEMENTATION.
  METHOD zif_excel_ixml_node_iterator~get_next.
    DATA lo_ixml_node TYPE REF TO if_ixml_node.

    lo_ixml_node = ixml_node_iterator->get_next( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_node_list IMPLEMENTATION.
  METHOD zif_excel_ixml_node_list~create_iterator.
    DATA lo_ixml_node_iterator TYPE REF TO if_ixml_node_iterator.

    lo_ixml_node_iterator = ixml_node_list->create_iterator( ).
    rval ?= lth_wrap_ixml=>wrap_ixml( lo_ixml_node_iterator ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_ostream_string IMPLEMENTATION.

ENDCLASS.


CLASS lth_wrap_ixml_parser IMPLEMENTATION.
  METHOD zif_excel_ixml_parser~add_strip_space_element.
    " Method PARSE_STRING of ZCL_EXCEL_THEME_FMT_SCHEME
    ixml_parser->add_strip_space_element( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~parse.
    ixml_parser->parse( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_normalizing.
    rval = ixml_parser->set_normalizing( is_normalizing ).
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_validating.
    rval = ixml_parser->set_validating( mode ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_renderer IMPLEMENTATION.
  METHOD zif_excel_ixml_renderer~render.
    rval = ixml_renderer->render( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_stream IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_stream_factory IMPLEMENTATION.
  METHOD zif_excel_ixml_stream_factory~create_istream_string.
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_istream_xstring.
* Method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007
*     lo_istream        = lo_streamfactory->create_istream_xstring( lv_content ).

    DATA lo_wrap_ixml_istream_string TYPE REF TO lth_wrap_ixml_istream_string.

    CREATE OBJECT lo_wrap_ixml_istream_string.
    lo_wrap_ixml_istream_string->lif_wrap_ixml_istream~ixml_istream = ixml_stream_factory->create_istream_xstring(
                                                                          string ).
    rval = lo_wrap_ixml_istream_string.
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_cstring.
    DATA lo_wrap_ixml_ostream_string TYPE REF TO lth_wrap_ixml_ostream_string.

    CREATE OBJECT lo_wrap_ixml_ostream_string.
    lo_wrap_ixml_ostream_string->lif_wrap_ixml_ostream~ixml_ostream = ixml_stream_factory->create_ostream_cstring(
                                                                          string->* ).
    rval = lo_wrap_ixml_ostream_string.
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_xstring.
    DATA lo_wrap_ixml_ostream_xstring TYPE REF TO lth_wrap_ixml_ostream_xstring.

    CREATE OBJECT lo_wrap_ixml_ostream_xstring.
*    lo_wrap_ixml_ostream_xstring->ixml_ostream_xstring = ixml_stream_factory->create_ostream_xstring( string->* ).
    lo_wrap_ixml_ostream_xstring->lif_wrap_ixml_ostream~ixml_ostream = ixml_stream_factory->create_ostream_xstring(
                                                                           string->* ).
    rval = lo_wrap_ixml_ostream_xstring.
  ENDMETHOD.
ENDCLASS.


CLASS lth_wrap_ixml_text IMPLEMENTATION.

ENDCLASS.


CLASS lth_wrap_ixml_unknown IMPLEMENTATION.
  METHOD zif_excel_ixml_unknown~query_interface.
  ENDMETHOD.
ENDCLASS.
