INTERFACE zif_excel_ixml_document
  PUBLIC .

  interfaces zif_excel_ixml_node.

  aliases APPEND_CHILD
    for zif_excel_ixml_node~APPEND_CHILD .
  aliases GET_FIRST_CHILD
    for zif_excel_ixml_node~GET_FIRST_CHILD .
  aliases SET_NAMESPACE_PREFIX
    for zif_excel_ixml_node~SET_NAMESPACE_PREFIX .

  methods CREATE_ELEMENT
    importing
      !NAME type STRING
      !NAMESPACE type STRING default ''
    returning
      value(RVAL) type ref to zif_excel_ixml_document .

  methods CREATE_SIMPLE_ELEMENT
    importing
      !NAME type STRING
      !NAMESPACE type STRING default ''
      !PARENT type ref to zIF_excel_IXML_NODE
      !VALUE type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_ELEMENT .

  methods CREATE_SIMPLE_ELEMENT_NS
    importing
      !NAME type STRING
      !PARENT type ref to zIF_excel_IXML_NODE
      !PREFIX type STRING default ''
      !URI type STRING default ''
      !VALUE type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_ELEMENT .

  methods FIND_FROM_NAME
    importing
      !DEPTH type I default 0
      !NAME type STRING
      !NAMESPACE type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_ELEMENT .

  methods FIND_FROM_NAME_NS
    importing
      !DEPTH type I default 0
      !NAME type STRING
      !URI type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_ELEMENT .

  methods GET_ELEMENTS_BY_TAG_NAME
    importing
      !DEPTH type I default 0
      !NAME type STRING
      !NAMESPACE type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_NODE_COLLECTION .

  methods GET_ELEMENTS_BY_TAG_NAME_NS
    importing
      !DEPTH type I default 0
      !NAME type STRING
      !URI type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_NODE_COLLECTION .

  methods GET_ROOT_ELEMENT
    returning
      value(RVAL) type ref to zIF_excel_IXML_ELEMENT .

  methods SET_ENCODING
    importing
      !ENCODING type ref to zIF_excel_IXML_ENCODING .

  methods SET_STANDALONE
    importing
      !STANDALONE type ABAP_BOOL .

ENDINTERFACE.
