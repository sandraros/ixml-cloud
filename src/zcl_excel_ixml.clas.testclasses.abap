*"* use this source file for your ABAP unit test classes

CLASS ltc_ixml DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA rc               TYPE i.
    DATA num_errors       TYPE i.
    DATA reason           TYPE string.
    DATA error            TYPE REF TO if_ixml_parse_error.
    DATA lv_xstring       TYPE xstring.
    DATA lo_ixml          TYPE REF TO if_ixml.
    DATA lo_streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA lo_istream       TYPE REF TO if_ixml_istream.
    DATA lo_parser        TYPE REF TO if_ixml_parser.
    DATA lo_element       TYPE REF TO if_ixml_element.
    DATA lv_string        TYPE string.
    DATA lo_document      TYPE REF TO if_ixml_document.
    METHODS end_tag_doesnt_match_begin_tag FOR TESTING RAISING cx_static_check.
    METHODS valid_most_simple_xml FOR TESTING RAISING cx_static_check.
    METHODS two_parsers FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_instances FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_stream_factories FOR TESTING RAISING cx_static_check.
    METHODS two_ixml_encodings FOR TESTING RAISING cx_static_check.
    METHODS get_children FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS find_from_name_ns FOR TESTING RAISING cx_static_check.
    METHODS append_child FOR TESTING RAISING cx_static_check.
    METHODS create_element FOR TESTING RAISING cx_static_check.
    METHODS create_simple_element FOR TESTING RAISING cx_static_check.
    METHODS create_simple_element_ns FOR TESTING RAISING cx_static_check.
    METHODS find_from_name FOR TESTING RAISING cx_static_check.
    METHODS get_elements_by_tag_name FOR TESTING RAISING cx_static_check.
    METHODS get_elements_by_tag_name_ns FOR TESTING RAISING cx_static_check.
    METHODS get_first_child FOR TESTING RAISING cx_static_check.
    METHODS get_root_element FOR TESTING RAISING cx_static_check.
    METHODS set_encoding FOR TESTING RAISING cx_static_check.
    METHODS set_namespace_prefix FOR TESTING RAISING cx_static_check.
    METHODS set_standalone FOR TESTING RAISING cx_static_check.

    DATA document         TYPE REF TO zif_excel_ixml_document.
    DATA element          TYPE REF TO zif_excel_ixml_element.
    DATA value            TYPE string.
    DATA ixml          TYPE REF TO zif_excel_ixml.
    DATA stream_factory TYPE REF TO zif_excel_ixml_stream_factory.
    DATA: xml TYPE string.

    METHODS parse
      IMPORTING
        xml_string TYPE csequence.

    METHODS render
        RETURNING
        VALUE(rv_xml_string) TYPE string.

    " class-methods class_setup.
    " class-methods class_teardown.
    " methods setup.
    " methods teardown.
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


CLASS ltc_sxml DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA node        TYPE REF TO if_sxml_node.
    DATA reader      TYPE REF TO if_sxml_reader.
    DATA xstring     TYPE xstring.
    DATA parse_error TYPE REF TO cx_sxml_parse_error.
    DATA error_rtti  TYPE REF TO cl_abap_typedescr.
    "! READER
    METHODS object_oriented_parsing FOR TESTING.
    METHODS empty_object_oriented_parsing FOR TESTING.
    METHODS token_based_parsing FOR TESTING.
    METHODS empty_token_based_parsing FOR TESTING.
    METHODS empty_xml FOR TESTING RAISING cx_static_check.
    METHODS invalid_xml FOR TESTING RAISING cx_static_check.
    METHODS invalid_xml_eof_reached FOR TESTING RAISING cx_static_check.
    METHODS invalid_xml_not_wellformed FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_ixml IMPLEMENTATION.
  METHOD end_tag_doesnt_match_begin_tag.
    lv_xstring = cl_abap_codepage=>convert_to( |<A></B>| ).

    lo_ixml = cl_ixml=>create( ).
    lo_streamfactory = lo_ixml->create_stream_factory( ).
    lo_istream = lo_streamfactory->create_istream_xstring( lv_xstring ).
    lo_document = lo_ixml->create_document( ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                        istream        = lo_istream
                                        document       = lo_document ).
*    lo_parser->set_normalizing( abap_true ).
*    lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
    rc = lo_parser->parse( ).
    cl_abap_unit_assert=>assert_equals( act = rc
                                        exp = lcl_ixml_factory=>ixml_mr-parser_error ).
    num_errors = lo_parser->num_errors( ).
    DO num_errors TIMES.
      error = lo_parser->get_error( index = sy-index - 1 ).
      reason = error->get_reason( ). " end tag 'B' does not match begin tag 'A'
    ENDDO.
  ENDMETHOD.

  METHOD get_children.
    lv_xstring = cl_abap_codepage=>convert_to( |<A attr="1">B</A>| ).

    lo_ixml = cl_ixml=>create( ).
    lo_streamfactory = lo_ixml->create_stream_factory( ).
    lo_istream = lo_streamfactory->create_istream_xstring( lv_xstring ).
    lo_document = lo_ixml->create_document( ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                        istream        = lo_istream
                                        document       = lo_document ).
    rc = lo_parser->parse( ).
    DATA(lo_children) = lo_document->get_root_element( )->get_children( ).
    DATA(lv_length) = lo_children->get_length( ).
    cl_abap_unit_assert=>assert_equals( act = lv_length
                                        exp = 1 ).
    DATA(lo_child) = lo_children->get_item( 0 ).
    DATA(lv_type) = lo_child->get_type( ).
    DATA(lv_name) = lo_child->get_name( ).
    DATA(lv_value) = lo_child->get_value( ).
    cl_abap_unit_assert=>assert_equals( act = lv_type
                                        exp = if_ixml_node=>co_node_text ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = '#text' ).
    cl_abap_unit_assert=>assert_equals( act = lv_value
                                        exp = 'B' ).
  ENDMETHOD.

  METHOD two_ixml_encodings.
    DATA lo_encoding   TYPE REF TO if_ixml_encoding.
    DATA lo_encoding_2 TYPE REF TO if_ixml_encoding.

    lo_ixml = cl_ixml=>create( ).
    lo_encoding = lo_ixml->create_encoding( byte_order    = if_ixml_encoding=>co_little_endian
                                            character_set = 'UTF-8' ).
    lo_encoding_2 = lo_ixml->create_encoding( byte_order    = if_ixml_encoding=>co_little_endian
                                              character_set = 'UTF-8' ).
    cl_abap_unit_assert=>assert_true( boolc( lo_encoding_2 <> lo_encoding ) ).
  ENDMETHOD.

  METHOD two_ixml_instances.
    DATA lo_ixml_2 TYPE REF TO if_ixml.

    lo_ixml = cl_ixml=>create( ).
    lo_ixml_2 = cl_ixml=>create( ).
    cl_abap_unit_assert=>assert_equals( act = lo_ixml_2
                                        exp = lo_ixml ).
  ENDMETHOD.

  METHOD two_ixml_stream_factories.
    DATA lo_streamfactory_2 TYPE REF TO if_ixml_stream_factory.

    lo_ixml = cl_ixml=>create( ).
    lo_streamfactory = lo_ixml->create_stream_factory( ).
    lo_streamfactory_2 = lo_ixml->create_stream_factory( ).
    cl_abap_unit_assert=>assert_true( boolc( lo_streamfactory_2 <> lo_streamfactory ) ).
  ENDMETHOD.

  METHOD two_parsers.
    DATA lo_istream_2 TYPE REF TO if_ixml_istream.

    lo_ixml = cl_ixml=>create( ).
    lo_document = lo_ixml->create_document( ).
    lo_streamfactory = lo_ixml->create_stream_factory( ).

    lv_xstring = cl_abap_codepage=>convert_to( |<B/>| ).
    lo_istream = lo_streamfactory->create_istream_xstring( lv_xstring ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                        istream        = lo_istream
                                        document       = lo_document ).
    lo_parser->set_normalizing( abap_true ).
    lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
    rc = lo_parser->parse( ).

    DATA lo_parser_2 TYPE REF TO if_ixml_parser.
    lv_string = '<A/>'.
    lo_istream_2 = lo_streamfactory->create_istream_string( lv_string ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                        istream        = lo_istream_2
                                        document       = lo_document ).
    rc = lo_parser->parse( ).

    lo_element = lo_document->get_root_element( ).
    lv_string = lo_element->get_name( ).

    " The second parsing is ignored
    cl_abap_unit_assert=>assert_equals( act = lv_string
                                        exp = 'B' ).
  ENDMETHOD.

  METHOD valid_most_simple_xml.
    DATA lv_content       TYPE xstring.
    DATA lo_ixml          TYPE REF TO if_ixml.
    DATA lo_streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA lo_istream       TYPE REF TO if_ixml_istream.
    DATA lo_parser        TYPE REF TO if_ixml_parser.

    lv_content = cl_abap_codepage=>convert_to( |<A/>| ).

    lo_ixml          = cl_ixml=>create( ).
    lo_streamfactory = lo_ixml->create_stream_factory( ).
    lo_istream       = lo_streamfactory->create_istream_xstring( lv_content ).
    DATA(lo_document) = lo_ixml->create_document( ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                        istream        = lo_istream
                                        document       = lo_document ).
    lo_parser->set_normalizing( abap_true ).
    lo_parser->set_validating( mode = zif_excel_ixml_parser=>co_no_validation ).
    rc = lo_parser->parse( ).
    " RC = 0
    num_errors = lo_parser->num_errors( ).
    " NUM_ERRORS = 0
  ENDMETHOD.
ENDCLASS.


CLASS ltc_main IMPLEMENTATION.
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
    DATA lo_ostream  TYPE REF TO zif_excel_ixml_ostream.
    DATA lo_renderer TYPE REF TO zif_excel_ixml_renderer.

    lo_ostream = stream_factory->create_ostream_cstring( rv_xml_string ).
    lo_renderer = ixml->create_renderer( ostream  = lo_ostream
                                            document = document ).
    xml = lo_renderer->render( ).
  ENDMETHOD.

  METHOD append_child.
    parse( '' ).
    DATA(lo_element) = document->create_element( name = 'A' ).
    document->append_child( lo_element ).
    render( ).
    cl_abap_unit_assert=>assert_equals( act = xml exp = '' ).
  ENDMETHOD.

  METHOD create_element.
*  element = document->create_element( ).
*  element = document->
  ENDMETHOD.

  METHOD create_simple_element.
*  element = document->create_simple_element( ).
  ENDMETHOD.

  METHOD create_simple_element_ns.
*  element = document->create_simple_element_ns( ).
  ENDMETHOD.

  METHOD find_from_name.
    element = document->find_from_name( name = 'elem' ).
  ENDMETHOD.

  METHOD find_from_name_ns.
    parse( |<elem/><a:elem xmlns:a="{ namespace-relationships }">A</a:elem>| ).
    element = document->find_from_name_ns( name = 'elem'
                                           uri  = namespace-relationships ).
    value = element->get_value( ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = 'A' ).
  ENDMETHOD.

  METHOD get_elements_by_tag_name.
*  element = document->get_elements_by_tag_name( ).
  ENDMETHOD.

  METHOD get_elements_by_tag_name_ns.
*  element = document->get_elements_by_tag_name_ns( ).
  ENDMETHOD.

  METHOD get_first_child.
*  element = document->get_first_child( ).
  ENDMETHOD.

  METHOD get_root_element.
*  element = document->get_root_element( ).
  ENDMETHOD.

  METHOD set_encoding.
*  element = document->set_encoding( ).
  ENDMETHOD.

  METHOD set_namespace_prefix.
*  element = document->set_namespace_prefix( ).
  ENDMETHOD.

  METHOD set_standalone.
*  element = document->set_standalone( ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_sxml IMPLEMENTATION.
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
ENDCLASS.
