*"* use this source file for your ABAP unit test classes


CLASS ltc_iisxml DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      ABSTRACT
      CREATE PROTECTED.

  PROTECTED SECTION.

    METHODS test_create_encoding.

    DATA ixml           TYPE REF TO zif_excel_ixml.
    DATA encoding       TYPE REF TO zif_excel_ixml_encoding.
    DATA document       TYPE REF TO zif_excel_ixml_document.
    DATA xstring        TYPE xstring.
    DATA ref_xstring    TYPE REF TO xstring.
    DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.
    DATA ostream        TYPE REF TO zif_excel_ixml_ostream.
    DATA renderer       TYPE REF TO zif_excel_ixml_renderer.
    DATA string         TYPE string.
    DATA rc             TYPE i.
    DATA num_errors     TYPE i.
    DATA reason         TYPE string.
*    DATA error          TYPE REF TO zif_excel_ixml_parse_error.

ENDCLASS.


CLASS ltc_isxml DEFINITION
      INHERITING FROM ltc_iisxml
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_encoding FOR TESTING.

    METHODS setup.

ENDCLASS.


CLASS ltc_isxml_document DEFINITION
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
    METHODS get_elements_by_tag_name_ns FOR TESTING RAISING cx_static_check.
    METHODS get_root_element FOR TESTING RAISING cx_static_check.
    METHODS set_encoding FOR TESTING RAISING cx_static_check.
    METHODS set_standalone FOR TESTING RAISING cx_static_check.

    CONSTANTS:
      "! Constant copied from the NAMESPACE constant of the protected section of ZCL_EXCEL_READER_2007
      BEGIN OF namespace,
        x14ac            TYPE string VALUE 'http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac',
        vba_project      TYPE string VALUE 'http://schemas.microsoft.com/office/2006/relationships/vbaProject' ##NEEDED, " for future incorporation of XLSM-reader
        c                TYPE string VALUE 'http://schemas.openxmlformats.org/drawingml/2006/chart',
        a                TYPE string VALUE 'http://schemas.openxmlformats.org/drawingml/2006/main',
        xdr              TYPE string VALUE 'http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing',
        mc               TYPE string VALUE 'http://schemas.openxmlformats.org/markup-compatibility/2006',
        r                TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships',
        chart            TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart',
        drawing          TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing',
        hyperlink        TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink',
        image            TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/image',
        office_document  TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument',
        printer_settings TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/printerSettings',
        shared_strings   TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings',
        styles           TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles',
        theme            TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme',
        worksheet        TYPE string VALUE 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet',
        relationships    TYPE string VALUE 'http://schemas.openxmlformats.org/package/2006/relationships',
        core_properties  TYPE string
                         VALUE 'http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties',
        main             TYPE string VALUE 'http://schemas.openxmlformats.org/spreadsheetml/2006/main',
      END OF namespace.
ENDCLASS.


CLASS ltc_isxml_node DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS append_child FOR TESTING RAISING cx_static_check.
    METHODS get_first_child FOR TESTING RAISING cx_static_check.
    METHODS set_namespace_prefix FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_ixml DEFINITION
      FOR TESTING
      INHERITING FROM ltc_iisxml
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_encoding FOR TESTING.

*    DATA:
*      BEGIN OF wrapped,
*        ixml TYPE REF TO if_ixml,
*      END OF wrapped.
*    DATA encoding       TYPE REF TO if_ixml_encoding.
*    DATA document       TYPE REF TO if_ixml_document.
*    DATA xstring        TYPE xstring.
*    DATA stream_factory TYPE REF TO if_ixml_stream_factory.
*    DATA ostream        TYPE REF TO if_ixml_ostream.
*    DATA renderer       TYPE REF TO if_ixml_renderer.
*    DATA string         TYPE string.
*    DATA rc             TYPE i.
*    DATA num_errors     TYPE i.
*    DATA reason         TYPE string.
*    DATA error          TYPE REF TO if_ixml_parse_error.

    METHODS setup.

ENDCLASS.


CLASS ltc_ixml_document DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS create_simple_element_ns FOR TESTING.

ENDCLASS.


CLASS ltc_ixml_node DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS get_children FOR TESTING RAISING cx_static_check.

    DATA lv_type     TYPE i.
    DATA lo_document TYPE REF TO if_ixml_document.
    DATA lo_children TYPE REF TO if_ixml_node_list.
    DATA lv_length   TYPE i.
    DATA lo_child    TYPE REF TO if_ixml_node.
    DATA lv_name     TYPE string.
    DATA lv_value    TYPE string.

    METHODS get_xml_document
      IMPORTING
        xml_string    TYPE string
      RETURNING
        VALUE(result) TYPE REF TO if_ixml_document.
ENDCLASS.


CLASS ltc_ixml_parser DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS empty_xml FOR TESTING RAISING cx_static_check.
    METHODS end_tag_doesnt_match_begin_tag FOR TESTING RAISING cx_static_check.
    METHODS most_simple_valid_xml FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_instances FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_stream_factories FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_encodings FOR TESTING RAISING cx_static_check.
    METHODS two_parsers FOR TESTING RAISING cx_static_check.

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


CLASS ltc_ixml_render DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS most_simple_valid_xml FOR TESTING RAISING cx_static_check.

    DATA rc             TYPE i.
    DATA num_errors     TYPE i.
    DATA reason         TYPE string.
    DATA error          TYPE REF TO if_ixml_parse_error.
    DATA xstring        TYPE xstring.
    DATA ixml           TYPE REF TO if_ixml.
    DATA stream_factory TYPE REF TO if_ixml_stream_factory.
    DATA ostream        TYPE REF TO if_ixml_ostream.
    DATA renderer       TYPE REF TO if_ixml_renderer.
    DATA element        TYPE REF TO if_ixml_element.
    DATA string         TYPE string.
    DATA document       TYPE REF TO if_ixml_document.
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
ENDCLASS.


CLASS ltc_sxml_writer DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS most_simple_valid_xml FOR TESTING RAISING cx_static_check.
    METHODS object_oriented_rendering FOR TESTING RAISING cx_static_check.
    METHODS token_based_rendering FOR TESTING RAISING cx_static_check.


    DATA node          TYPE REF TO if_sxml_node.
    DATA open_element  TYPE REF TO if_sxml_open_element.
    DATA writer        TYPE REF TO if_sxml_writer.
    DATA string_writer TYPE REF TO cl_sxml_string_writer.
    DATA string        TYPE string.
    DATA xstring       TYPE xstring.
    DATA parse_error   TYPE REF TO cx_sxml_parse_error.
    DATA error_rtti    TYPE REF TO cl_abap_typedescr.
    DATA value_node    TYPE REF TO if_sxml_value_node.
    DATA close_element TYPE REF TO if_sxml_close_element.
ENDCLASS.


INTERFACE lif_iisxml_all_friends.
ENDINTERFACE.


INTERFACE lif_iisxml_istream.
  INTERFACES zif_excel_ixml_istream.
  DATA ixml_istream TYPE REF TO if_ixml_istream.
ENDINTERFACE.


INTERFACE lif_iisxml_ostream.
  INTERFACES zif_excel_ixml_ostream.
  DATA ixml_ostream TYPE REF TO if_ixml_ostream.
ENDINTERFACE.


*CLASS lth_iisxml_root_all DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES lif_iisxml_all_friends.
*ENDCLASS.


CLASS lth_iisxml_unknown DEFINITION
*    INHERITING FROM lth_iisxml_root_all
    FOR TESTING
    CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_unknown.
    INTERFACES lif_iisxml_all_friends.

ENDCLASS.


CLASS lth_iisxml_node DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PROTECTED
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node.

  PRIVATE SECTION.

    DATA ixml_node TYPE REF TO if_ixml_node.

ENDCLASS.


CLASS lth_iisxml DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_excel_ixml.

  PRIVATE SECTION.

    CLASS-DATA singleton TYPE REF TO lth_iisxml.
    DATA ixml TYPE REF TO if_ixml.

ENDCLASS.


CLASS lth_iisxml_attribute DEFINITION
    INHERITING FROM lth_iisxml_node
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_attribute.

ENDCLASS.


CLASS lth_iisxml_character_data DEFINITION
    INHERITING FROM lth_iisxml_node
    FOR TESTING
    CREATE PROTECTED
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_character_data.

ENDCLASS.


CLASS lth_iisxml_document DEFINITION
    INHERITING FROM lth_iisxml_node
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_document.

  PRIVATE SECTION.

    DATA ixml_document TYPE REF TO if_ixml_document.

ENDCLASS.


CLASS lth_iisxml_element DEFINITION
    INHERITING FROM lth_iisxml_node
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_element.

  PRIVATE SECTION.

    DATA ixml_element TYPE REF TO if_ixml_element.

ENDCLASS.


CLASS lth_iisxml_encoding DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_encoding.

  PRIVATE SECTION.

    DATA ixml_encoding TYPE REF TO if_ixml_encoding.

ENDCLASS.


CLASS lth_iisxml_istream_string DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES lif_iisxml_istream.

ENDCLASS.


CLASS lth_iisxml_istream_xstring DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES lif_iisxml_istream.

ENDCLASS.


CLASS lth_iisxml_named_node_map DEFINITION
    CREATE PRIVATE
    FRIENDS lth_iisxml_document.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_named_node_map.

ENDCLASS.


CLASS lth_iisxml_node_collection DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_collection.

ENDCLASS.


CLASS lth_iisxml_node_iterator DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_iterator.

ENDCLASS.


CLASS lth_iisxml_node_list DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_node_list.

ENDCLASS.


CLASS lth_iisxml_ostream_string DEFINITION
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_iisxml_ostream.

ENDCLASS.


CLASS lth_iisxml_ostream_xstring DEFINITION
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES lif_iisxml_ostream.

*  PRIVATE SECTION.
*
*    DATA ixml_ostream_xstring TYPE REF TO if_ixml_ostream.

ENDCLASS.


CLASS lth_iisxml_parser DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_parser.

  PRIVATE SECTION.

    DATA ixml_parser TYPE REF TO if_ixml_parser.

ENDCLASS.


CLASS lth_iisxml_renderer DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_renderer.

  PRIVATE SECTION.

    DATA ixml_renderer TYPE REF TO if_ixml_renderer.

ENDCLASS.


CLASS lth_iisxml_stream DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream.
ENDCLASS.


CLASS lth_iisxml_stream_factory DEFINITION
    INHERITING FROM lth_iisxml_unknown
    FOR TESTING
    CREATE PRIVATE
    FRIENDS lif_iisxml_all_friends.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_stream_factory.

  PRIVATE SECTION.

    DATA ixml_stream_factory TYPE REF TO if_ixml_stream_factory.

ENDCLASS.


CLASS lth_iisxml_text DEFINITION
    INHERITING FROM lth_iisxml_character_data
    FOR TESTING
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_excel_ixml_text.

ENDCLASS.


CLASS lth_isxml DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA document       TYPE REF TO zif_excel_ixml_document.
    CLASS-DATA element        TYPE REF TO zif_excel_ixml_element.
    CLASS-DATA value          TYPE string.
    CLASS-DATA ixml           TYPE REF TO zif_excel_ixml.
    CLASS-DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.
    CLASS-DATA xml_string     TYPE string.
    CLASS-DATA xml_xstring    TYPE xstring.

    CLASS-METHODS parse
      IMPORTING
        xml_string TYPE csequence.

    CLASS-METHODS render
      RETURNING
        VALUE(rv_result) TYPE string.

    CLASS-METHODS create_document.

ENDCLASS.


CLASS lth_ixml DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA document       TYPE REF TO if_ixml_document.
    CLASS-DATA element        TYPE REF TO if_ixml_element.
    CLASS-DATA value          TYPE string.
    CLASS-DATA ixml           TYPE REF TO if_ixml.
    CLASS-DATA stream_factory TYPE REF TO if_ixml_stream_factory.
    CLASS-DATA xml_string     TYPE string.
    CLASS-DATA xml_xstring    TYPE xstring.

    CLASS-METHODS parse
      IMPORTING
        xml_string TYPE csequence.

    CLASS-METHODS render
      RETURNING
        VALUE(rv_result) TYPE string.

    CLASS-METHODS create_document.

ENDCLASS.


CLASS ltc_isxml_document IMPLEMENTATION.
  METHOD create_element.
    lth_isxml=>create_document( ).
    lth_isxml=>element = lth_isxml=>document->create_element( name = 'A' ).
    lth_isxml=>document->append_child( lth_isxml=>element ).
    lth_isxml=>xml_string = lth_isxml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = lth_isxml=>xml_string
                                        exp = '<A/>' ).
  ENDMETHOD.

  METHOD create_simple_element.
    lth_isxml=>create_document( ).
    lth_isxml=>document->create_simple_element( name   = 'A'
                                                parent = lth_isxml=>document ).
    lth_isxml=>xml_string = lth_isxml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = lth_isxml=>xml_string
                                        exp = '<A/>' ).
  ENDMETHOD.

  METHOD create_simple_element_ns.
    lth_isxml=>create_document( ).
    lth_isxml=>document->create_simple_element_ns( name   = 'A'
                                                   parent = lth_isxml=>document
                                                   prefix = 'a' ).
    lth_isxml=>xml_string = lth_isxml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = lth_isxml=>xml_string
                                        exp = '<a:A/>' ).
  ENDMETHOD.

  METHOD find_from_name.
    lth_isxml=>element = lth_isxml=>document->find_from_name( name = 'elem' ).
  ENDMETHOD.

  METHOD find_from_name_ns.
    lth_isxml=>parse( |<elem/><a:elem xmlns:a="{ namespace-relationships }">A</a:elem>| ).
    lth_isxml=>element = lth_isxml=>document->find_from_name_ns( name = 'elem'
                                                                 uri  = namespace-relationships ).
    lth_isxml=>value = lth_isxml=>element->get_value( ).
    cl_abap_unit_assert=>assert_equals( act = lth_isxml=>value
                                        exp = 'A' ).
  ENDMETHOD.

  METHOD get_elements_by_tag_name.
*  element = document->get_elements_by_tag_name( ).
  ENDMETHOD.

  METHOD get_elements_by_tag_name_ns.
*  element = document->get_elements_by_tag_name_ns( ).
  ENDMETHOD.

  METHOD get_root_element.
*  element = document->get_root_element( ).
  ENDMETHOD.

  METHOD set_encoding.
*  element = document->set_encoding( ).
  ENDMETHOD.

  METHOD set_standalone.
*  element = document->set_standalone( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_isxml_node IMPLEMENTATION.
  METHOD append_child.
    lth_isxml=>parse( '<ROOT/>' ).
    DATA(lo_element) = lth_isxml=>document->get_root_element( ).
    DATA(lo_element_2) = lth_isxml=>document->create_element( name = 'A' ).
    lo_element->append_child( lo_element_2 ).
    lth_isxml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = lth_isxml=>xml_string
                                        exp = '' ).
  ENDMETHOD.

  METHOD get_first_child.
*  element = document->get_first_child( ).
  ENDMETHOD.

  METHOD set_namespace_prefix.
*  element = document->set_namespace_prefix( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_iisxml IMPLEMENTATION.
  METHOD test_create_encoding.
*ZCL_EXCEL_THEME
*    lo_encoding = lo_ixml->create_encoding( byte_order = if_ixml_encoding=>co_platform_endian
*                                            character_set = 'UTF-8' ).
*    lo_document = lo_ixml->create_document( ).
*    lo_document->set_encoding( lo_encoding ).

    document = ixml->create_document( ).
    encoding = ixml->create_encoding( byte_order    = if_ixml_encoding=>co_platform_endian
                                      character_set = 'UTF-8' ).
    document->set_encoding( encoding ).
    stream_factory = ixml->create_stream_factory( ).
    GET REFERENCE OF xstring INTO ref_xstring.
    ostream = stream_factory->create_ostream_xstring( ref_xstring ).
    renderer = ixml->create_renderer( ostream  = ostream
                                      document = document ).
    document->create_simple_element( name   = 'é'
                                     parent = document ).
    rc = renderer->render( ).
    string = cl_abap_codepage=>convert_from( xstring ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<?xml version="1.0" encoding="utf-8"?><é/>' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_isxml IMPLEMENTATION.
  METHOD create_encoding.
    test_create_encoding( ).
  ENDMETHOD.

  METHOD setup.
    ixml = zcl_excel_ixml=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml IMPLEMENTATION.
  METHOD create_encoding.
    test_create_encoding( ).
  ENDMETHOD.

  METHOD setup.
    ixml = lth_iisxml=>create( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_document IMPLEMENTATION.
  METHOD create_simple_element_ns.
    lth_ixml=>create_document( ).
    lth_ixml=>document->create_simple_element_ns( name   = 'A'
                                                  parent = lth_ixml=>document
                                                  prefix = 'a' ).
    lth_ixml=>xml_string = lth_ixml=>render( ).
    cl_abap_unit_assert=>assert_equals( act = lth_ixml=>xml_string
                                        exp = '<a:A/>' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_node IMPLEMENTATION.
  METHOD get_children.
    " GIVEN
    lo_document = get_xml_document( |<A attr="1">B</A>| ).
    " WHEN
    lo_children = lo_document->get_root_element( )->get_children( ).
    " THEN
    lv_length = lo_children->get_length( ).
    cl_abap_unit_assert=>assert_equals( act = lv_length
                                        exp = 1 ).
    lo_child = lo_children->get_item( 0 ).
    lv_type = lo_child->get_type( ).
    lv_name = lo_child->get_name( ).
    lv_value = lo_child->get_value( ).
    cl_abap_unit_assert=>assert_equals( act = lv_type
                                        exp = if_ixml_node=>co_node_text ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = '#text' ).
    cl_abap_unit_assert=>assert_equals( act = lv_value
                                        exp = 'B' ).
  ENDMETHOD.

  METHOD get_xml_document.
    DATA(lv_xstring) = cl_abap_codepage=>convert_to( xml_string ).

    DATA(lo_ixml) = cl_ixml=>create( ).
    DATA(lo_streamfactory) = lo_ixml->create_stream_factory( ).
    DATA(lo_istream) = lo_streamfactory->create_istream_xstring( lv_xstring ).
    result = lo_ixml->create_document( ).
    DATA(lo_parser) = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                              istream        = lo_istream
                                              document       = result ).
    DATA(rc) = lo_parser->parse( ).
    cl_abap_unit_assert=>assert_equals( act = rc
                                        exp = lcl_isxml=>ixml_mr-dom_ok ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_parser IMPLEMENTATION.
  METHOD empty_xml.
    ixml = cl_ixml=>create( ).
    stream_factory = ixml->create_stream_factory( ).
    xstring = VALUE #( ).
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
    xstring = cl_abap_codepage=>convert_to( |<A></B>| ).

    ixml = cl_ixml=>create( ).
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

  METHOD two_ixml_encodings.
    DATA lo_encoding   TYPE REF TO if_ixml_encoding.
    DATA lo_encoding_2 TYPE REF TO if_ixml_encoding.

    ixml = cl_ixml=>create( ).
    lo_encoding = ixml->create_encoding( byte_order    = if_ixml_encoding=>co_little_endian
                                         character_set = 'UTF-8' ).
    lo_encoding_2 = ixml->create_encoding( byte_order    = if_ixml_encoding=>co_little_endian
                                           character_set = 'UTF-8' ).
    cl_abap_unit_assert=>assert_true( boolc( lo_encoding_2 <> lo_encoding ) ).
  ENDMETHOD.

  METHOD two_ixml_instances.
    DATA lo_ixml_2 TYPE REF TO if_ixml.

    ixml = cl_ixml=>create( ).
    lo_ixml_2 = cl_ixml=>create( ).
    cl_abap_unit_assert=>assert_equals( act = lo_ixml_2
                                        exp = ixml ).
  ENDMETHOD.

  METHOD two_ixml_stream_factories.
    DATA lo_streamfactory_2 TYPE REF TO if_ixml_stream_factory.

    ixml = cl_ixml=>create( ).
    stream_factory = ixml->create_stream_factory( ).
    lo_streamfactory_2 = ixml->create_stream_factory( ).
    cl_abap_unit_assert=>assert_true( boolc( lo_streamfactory_2 <> stream_factory ) ).
  ENDMETHOD.

  METHOD two_parsers.
    DATA lo_istream_2 TYPE REF TO if_ixml_istream.
    DATA lo_parser_2  TYPE REF TO if_ixml_parser.

    ixml = cl_ixml=>create( ).
    document = ixml->create_document( ).
    stream_factory = ixml->create_stream_factory( ).

    xstring = cl_abap_codepage=>convert_to( |<B/>| ).
    istream = stream_factory->create_istream_xstring( xstring ).
    parser = ixml->create_parser( stream_factory = stream_factory
                                  istream        = istream
                                  document       = document ).
    parser->set_normalizing( abap_true ).
    parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
    rc = parser->parse( ).

    string = '<A/>'.
    lo_istream_2 = stream_factory->create_istream_string( string ).
    parser = ixml->create_parser( stream_factory = stream_factory
                                  istream        = lo_istream_2
                                  document       = document ).
    rc = parser->parse( ).

    element = document->get_root_element( ).
    string = element->get_name( ).

    " The second parsing is ignored
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = 'B' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_ixml_render IMPLEMENTATION.
  METHOD most_simple_valid_xml.
    ixml = cl_ixml=>create( ).
    document = ixml->create_document( ).
    element = document->create_simple_element( name   = 'ROOT'
                                               parent = document ).
    stream_factory = ixml->create_stream_factory( ).
    ostream = stream_factory->create_ostream_xstring( xstring ).
    renderer = ixml->create_renderer( ostream  = ostream
                                      document = document ).
    document->set_declaration( abap_false ).
    rc = renderer->render( ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_codepage=>convert_from( xstring )
                                        exp = '<ROOT/>' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_sxml_reader IMPLEMENTATION.
  METHOD empty_object_oriented_parsing.
    DATA(xstring) = cl_abap_codepage=>convert_to( '<ROOTNODE/>' ).
    DATA(reader) = cl_sxml_string_reader=>create( xstring ).
    DATA(node) = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_element_open ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( node IS INSTANCE OF cl_sxml_open_element ) ).
    DATA(node_open) = CAST if_sxml_open_element( node ).
    cl_abap_unit_assert=>assert_equals( act = node_open->qname
                                        exp = VALUE qname( name = 'ROOTNODE' ) ).
    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_element_close ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( node IS INSTANCE OF cl_sxml_close_element ) ).
    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_not_bound( node ).
  ENDMETHOD.

  METHOD empty_token_based_parsing.
    DATA(xstring) = cl_abap_codepage=>convert_to( '<ROOTNODE/>' ).
    DATA(reader) = cl_sxml_string_reader=>create( xstring ).
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

    xstring = VALUE xstring( )."cl_abap_codepage=>convert_to( '' ).
    reader = cl_sxml_string_reader=>create( xstring ).
    TRY.
        node = reader->read_next_node( ).
        cl_abap_unit_assert=>fail( msg = 'should have failed' ).
      CATCH cx_root INTO DATA(error).
        DATA(error_rtti) = cl_abap_typedescr=>describe_by_object_ref( error ).
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
      CATCH cx_root INTO DATA(error).
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
      CATCH cx_root INTO DATA(error).
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
    DATA(xstring) = cl_abap_codepage=>convert_to( '<ROOTNODE ATTR="Efe=">Efe=</ROOTNODE>' ).
    DATA(reader) = cl_sxml_string_reader=>create( xstring ).

    DATA(node) = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_element_open ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( node IS INSTANCE OF cl_sxml_open_element ) ).
    DATA(node_open) = CAST if_sxml_open_element( node ).
    cl_abap_unit_assert=>assert_equals( act = node_open->qname
                                        exp = VALUE qname( name = 'ROOTNODE' ) ).

    DATA(node_attr) = node_open->get_attribute_value( 'ATTR' ).
    cl_abap_unit_assert=>assert_bound( node_attr ).
    cl_abap_unit_assert=>assert_equals( act = node_attr->type
                                        exp = node_attr->co_vt_text ).
    cl_abap_unit_assert=>assert_equals( act = node_attr->get_value( )
                                        exp = 'Efe=' ).
    cl_abap_unit_assert=>assert_equals( act = node_attr->get_value_raw( )
                                        exp = CONV xstring( 'E0' ) ).

    node = reader->read_current_node( ).

    node = reader->read_next_node( ).
    cl_abap_unit_assert=>assert_bound( node ).
    cl_abap_unit_assert=>assert_equals( act = node->type
                                        exp = node->co_nt_value ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( node IS INSTANCE OF cl_sxml_value ) ).
    DATA(value_node) = CAST if_sxml_value_node( node ).
    cl_abap_unit_assert=>assert_equals( act = value_node->get_value( )
                                        exp = 'Efe=' ).
    cl_abap_unit_assert=>assert_equals( act = value_node->get_value_raw( )
                                        exp = CONV xstring( 'E0' ) ).
  ENDMETHOD.

  METHOD token_based_parsing.
    DATA(xstring) = cl_abap_codepage=>convert_to( '<ROOTNODE ATTR="UFE=">UFE=</ROOTNODE>' ).
    DATA(reader) = cl_sxml_string_reader=>create( xstring ).
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
    cl_abap_unit_assert=>assert_equals( act = reader->value_raw
                                        exp = VALUE xstring( ) ).

    reader->next_attribute( ).
    cl_abap_unit_assert=>assert_equals( act = reader->node_type
                                        exp = if_sxml_node=>co_nt_element_open ). " i.e. NOT if_sxml_node=>co_nt_attribute
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
    cl_abap_unit_assert=>assert_equals( act = reader->value_raw
                                        exp = VALUE xstring( ) ).

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
    DATA(xstring) = cl_abap_codepage=>convert_to( '<?xml version="1.0" encoding="utf-8" standalone="yes"?><ROOT/>' ).
    DATA(reader) = cl_sxml_string_reader=>create( xstring ).
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
  METHOD most_simple_valid_xml.
    writer = cl_sxml_string_writer=>create( ).
    open_element = writer->new_open_element( 'A' ).
    writer->write_node( open_element ).
    close_element = writer->new_close_element( ).
    writer->write_node( close_element ).

    string_writer ?= writer.
    xstring = string_writer->get_output( ).
    string = cl_abap_codepage=>convert_from( xstring ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<A/>' ).
  ENDMETHOD.

  METHOD object_oriented_rendering.
    " WRITE_NODE and NEW_* methods
    " NB: NEW_* methods are static methods (i.e. it's valid: DATA(open_element) = cl_sxml_writer=>if_sxml_writer~new_open_element( 'ROOTNODE' ).)
    writer = cl_sxml_string_writer=>create( ).
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

    string_writer ?= writer.
    xstring = string_writer->get_output( ).
    string = cl_abap_codepage=>convert_from( xstring ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<ROOTNODE ATTR="5" ATTR2="A" ATTR3="UFE=">HELLO</ROOTNODE>' ).
  ENDMETHOD.

  METHOD token_based_rendering.
    writer = cl_sxml_string_writer=>create( ).
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

    string_writer ?= writer.
    xstring = string_writer->get_output( ).
    string = cl_abap_codepage=>convert_from( xstring ).
    cl_abap_unit_assert=>assert_equals( act = string
                                        exp = '<ROOTNODE ATTR="5" ATTR2="UFE=">HELLO<NODE>UFE=</NODE></ROOTNODE>' ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml IMPLEMENTATION.
  METHOD create.
    IF singleton IS NOT BOUND.
      singleton = NEW lth_iisxml( ).
      singleton->ixml = cl_ixml=>create( ).
    ENDIF.
    ro_result = singleton.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_document.
    DATA lo_iisxml_document TYPE REF TO lth_iisxml_document.
    DATA lo_iisxml_document_as_node TYPE REF TO lth_iisxml_node.

    lo_iisxml_document = NEW lth_iisxml_document( ).
    lo_iisxml_document->ixml_document = ixml->create_document( ).
    lo_iisxml_document_as_node = lo_iisxml_document.
    lo_iisxml_document_as_node->ixml_node = lo_iisxml_document->ixml_document.

    rval = lo_iisxml_document.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_encoding.
    DATA encoding TYPE REF TO lth_iisxml_encoding.

    encoding = NEW lth_iisxml_encoding( ).
    encoding->ixml_encoding = ixml->create_encoding( byte_order    = byte_order
                                                     character_set = character_set ).
    rval = encoding.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_parser.
    DATA parser                TYPE REF TO lth_iisxml_parser.
    DATA iisxml_document       TYPE REF TO lth_iisxml_document.
    DATA iisxml_istream        TYPE REF TO lif_iisxml_istream.
    DATA iisxml_stream_factory TYPE REF TO lth_iisxml_stream_factory.

    parser = NEW lth_iisxml_parser( ).
    iisxml_document ?= document.
    iisxml_istream ?= istream.
    iisxml_stream_factory ?= stream_factory.
    parser->ixml_parser = ixml->create_parser( document       = iisxml_document->ixml_document
                                               istream        = iisxml_istream->ixml_istream
                                               stream_factory = iisxml_stream_factory->ixml_stream_factory ).
    rval = parser.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_renderer.
    DATA lo_renderer     TYPE REF TO lth_iisxml_renderer.
    DATA iisxml_document TYPE REF TO lth_iisxml_document.
    DATA iisxml_ostream  TYPE REF TO lif_iisxml_ostream.

    lo_renderer = NEW lth_iisxml_renderer( ).
    iisxml_document ?= document.
    iisxml_ostream ?= ostream.
    lo_renderer->ixml_renderer ?= ixml->create_renderer( document = iisxml_document->ixml_document
                                                         ostream  = iisxml_ostream->ixml_ostream ).
    rval = lo_renderer.
  ENDMETHOD.

  METHOD zif_excel_ixml~create_stream_factory.
    DATA lo_stream_factory TYPE REF TO lth_iisxml_stream_factory.

    lo_stream_factory = NEW lth_iisxml_stream_factory( ).
    lo_stream_factory->ixml_stream_factory = ixml->create_stream_factory( ).
    rval = lo_stream_factory.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_document IMPLEMENTATION.
  METHOD zif_excel_ixml_document~create_element.
*ixml_document->create_element( ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element.
    DATA lo_iisxml_element TYPE REF TO lth_iisxml_element.
    DATA lo_iisxml_parent  TYPE REF TO lth_iisxml_node.
    DATA lo_iisxml_element_as_node TYPE REF TO lth_iisxml_node.

    CREATE OBJECT lo_iisxml_element.
    lo_iisxml_parent ?= parent.
    lo_iisxml_element->ixml_element = ixml_document->create_simple_element( name   = name
                                                                            parent = lo_iisxml_parent->ixml_node ).
    lo_iisxml_element_as_node = lo_iisxml_element.
    lo_iisxml_element_as_node->ixml_node = lo_iisxml_element->ixml_element.
    rval = lo_iisxml_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_document~create_simple_element_ns.
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
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_encoding.
    DATA iisxml_encoding TYPE REF TO lth_iisxml_encoding.

    iisxml_encoding ?= encoding.
    ixml_document->set_encoding( encoding = iisxml_encoding->ixml_encoding ).
  ENDMETHOD.

  METHOD zif_excel_ixml_document~set_standalone.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_element IMPLEMENTATION.
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


CLASS lth_iisxml_istream_string IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_istream_xstring IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_named_node_map IMPLEMENTATION.
  METHOD zif_excel_ixml_named_node_map~create_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_node IMPLEMENTATION.
  METHOD zif_excel_ixml_node~append_child.
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
  ENDMETHOD.

  METHOD zif_excel_ixml_node~get_value.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_namespace_prefix.
  ENDMETHOD.

  METHOD zif_excel_ixml_node~set_value.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_node_collection IMPLEMENTATION.
  METHOD zif_excel_ixml_node_collection~create_iterator.
  ENDMETHOD.

  METHOD zif_excel_ixml_node_collection~get_length.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_node_iterator IMPLEMENTATION.
  METHOD zif_excel_ixml_node_iterator~get_next.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_node_list IMPLEMENTATION.
  METHOD zif_excel_ixml_node_list~create_iterator.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_ostream_string IMPLEMENTATION.

ENDCLASS.


CLASS lth_iisxml_parser IMPLEMENTATION.
  METHOD zif_excel_ixml_parser~add_strip_space_element.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~parse.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_normalizing.
  ENDMETHOD.

  METHOD zif_excel_ixml_parser~set_validating.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_renderer IMPLEMENTATION.
  METHOD zif_excel_ixml_renderer~render.
    rval = ixml_renderer->render( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_stream IMPLEMENTATION.
  METHOD zif_excel_ixml_stream~close.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_stream_factory IMPLEMENTATION.
  METHOD zif_excel_ixml_stream_factory~create_istream_string.
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_istream_xstring.
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_cstring.
  ENDMETHOD.

  METHOD zif_excel_ixml_stream_factory~create_ostream_xstring.
    DATA lo_iisxml_ostream_xstring TYPE REF TO lth_iisxml_ostream_xstring.

    CREATE OBJECT lo_iisxml_ostream_xstring.
*    lo_iisxml_ostream_xstring->ixml_ostream_xstring = ixml_stream_factory->create_ostream_xstring( string->* ).
    lo_iisxml_ostream_xstring->lif_iisxml_ostream~ixml_ostream = ixml_stream_factory->create_ostream_xstring( string->* ).
    rval = lo_iisxml_ostream_xstring.
  ENDMETHOD.
ENDCLASS.


CLASS lth_iisxml_text IMPLEMENTATION.

ENDCLASS.


CLASS lth_iisxml_unknown IMPLEMENTATION.
  METHOD zif_excel_ixml_unknown~query_interface.
  ENDMETHOD.
ENDCLASS.


CLASS lth_isxml IMPLEMENTATION.
  METHOD create_document.
    ixml = zcl_excel_ixml=>create( ).
    document = ixml->create_document( ).
  ENDMETHOD.

  METHOD parse.
    DATA lv_xstring TYPE xstring.
    DATA lo_istream TYPE REF TO zif_excel_ixml_istream.
    DATA lo_parser  TYPE REF TO zif_excel_ixml_parser.

    " code inspired from the method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007.
    ixml = zcl_excel_ixml=>create( ).
    document = ixml->create_document( ).
    stream_factory = ixml->create_stream_factory( ).

    lv_xstring = cl_abap_codepage=>convert_to( '<root>' && xml_string && '</root>' ).
    lo_istream = stream_factory->create_istream_xstring( lv_xstring ).
    lo_parser = ixml->create_parser( stream_factory = stream_factory
                                     istream        = lo_istream
                                     document       = document ).
    lo_parser->set_normalizing( abap_true ).
    lo_parser->set_validating( mode = zif_excel_ixml_parser=>co_no_validation ).
    lo_parser->parse( ).
  ENDMETHOD.

  METHOD render.
    DATA lr_string   TYPE REF TO string.
    DATA lo_ostream  TYPE REF TO zif_excel_ixml_ostream.
    DATA lo_renderer TYPE REF TO zif_excel_ixml_renderer.

    stream_factory = ixml->create_stream_factory( ).
    GET REFERENCE OF rv_result INTO lr_string.
    lo_ostream = stream_factory->create_ostream_cstring( lr_string ).
    lo_renderer = ixml->create_renderer( ostream  = lo_ostream
                                         document = document ).
    " Fills RV_RESULT
    lo_renderer->render( ).
  ENDMETHOD.
ENDCLASS.


CLASS lth_ixml IMPLEMENTATION.
  METHOD create_document.
    ixml = cl_ixml=>create( ).
    document = ixml->create_document( ).
    document->set_declaration( abap_false ).
  ENDMETHOD.

  METHOD parse.
    DATA lv_xstring TYPE xstring.
    DATA lo_istream TYPE REF TO if_ixml_istream.
    DATA lo_parser  TYPE REF TO if_ixml_parser.

    " code inspired from the method GET_IXML_FROM_ZIP_ARCHIVE of ZCL_EXCEL_READER_2007.
    ixml = cl_ixml=>create( ).
    document = ixml->create_document( ).
    stream_factory = ixml->create_stream_factory( ).

    lv_xstring = cl_abap_codepage=>convert_to( '<root>' && xml_string && '</root>' ).
    lo_istream = stream_factory->create_istream_xstring( lv_xstring ).
    lo_parser = ixml->create_parser( stream_factory = stream_factory
                                     istream        = lo_istream
                                     document       = document ).
    lo_parser->set_normalizing( abap_true ).
    lo_parser->set_validating( mode = zif_excel_ixml_parser=>co_no_validation ).
    lo_parser->parse( ).
  ENDMETHOD.

  METHOD render.
    DATA lo_ostream  TYPE REF TO if_ixml_ostream.
    DATA lo_renderer TYPE REF TO if_ixml_renderer.

    stream_factory = ixml->create_stream_factory( ).
    lo_ostream = stream_factory->create_ostream_cstring( rv_result ).
    lo_renderer = ixml->create_renderer( ostream  = lo_ostream
                                         document = document ).
    " Fills RV_RESULT
    lo_renderer->render( ).
  ENDMETHOD.
ENDCLASS.
