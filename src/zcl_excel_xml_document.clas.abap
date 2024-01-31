CLASS zcl_excel_xml_document DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA m_document TYPE REF TO zif_excel_ixml_document READ-ONLY.

    METHODS parse_string
      IMPORTING
        !stream TYPE string
      RETURNING
        VALUE(retcode) TYPE sysubrc.

    METHODS parse_xstring
      IMPORTING
        !stream TYPE xstring
      RETURNING
        VALUE(retcode) TYPE sysubrc.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA g_ixml           TYPE REF TO zif_excel_ixml.
    CLASS-DATA g_stream_factory TYPE REF TO zif_excel_ixml_stream_factory.

    METHODS parse
      IMPORTING
        !stream TYPE REF TO zif_excel_ixml_istream
      RETURNING
        VALUE(retcode) TYPE sysubrc.

ENDCLASS.


CLASS zcl_excel_xml_document IMPLEMENTATION.
  METHOD parse.
    DATA l_parser TYPE REF TO zif_excel_ixml_parser.
    DATA l_errno  TYPE i.

*    retcode = c_no_ixml.

    IF g_ixml IS INITIAL.
      RETURN.
    ENDIF.
    " -- create the XML tree for the input
    m_document = g_ixml->create_document( ).

    IF m_document IS INITIAL.
      RETURN.
    ENDIF.
    IF stream IS INITIAL.
      RETURN.
    ENDIF.

*    " --- note 2132282 ---
*    IF m_dtd_expansion > 0.
*      stream->set_dtd_restriction( level = if_ixml_istream=>dtd_restricted ).
*      stream->set_max_expansion( m_dtd_expansion ).
*    ENDIF.
*    "  ---

    l_parser = g_ixml->create_parser( stream_factory = g_stream_factory
                                      istream        = stream
                                      document       = m_document ).

    " --- default PARSER omits leading spaces 03.12.02 ---
    l_parser->set_normalizing( is_normalizing = space ).

    l_parser->parse( ).
*    retcode = l_parser->parse( ).

*    IF retcode NE 0.
*      l_errno = l_parser->num_errors( ).
*      IF retcode >= l_errno.
*        retcode = l_errno.
*      ENDIF.
*      m_parse_error = l_parser->get_error( index = 0 ).
*    ENDIF.
  ENDMETHOD.

  METHOD parse_string.
    DATA: l_stream TYPE REF TO zif_excel_ixml_istream.

*    retcode = c_no_ixml.

    IF g_stream_factory IS INITIAL.
      RETURN.
    ENDIF.

    l_stream = g_stream_factory->create_istream_string( string = stream ).

    retcode = parse( stream = l_stream ).

    l_stream->close( ).
  ENDMETHOD.

  METHOD parse_xstring.
    DATA: l_stream TYPE REF TO zif_excel_ixml_istream.

*    retcode = c_no_ixml.

    IF g_stream_factory IS INITIAL.
      RETURN.
    ENDIF.

    l_stream = g_stream_factory->create_istream_xstring( string = stream ).

    retcode = parse( stream = l_stream ).

    l_stream->close( ).
  ENDMETHOD.
ENDCLASS.
