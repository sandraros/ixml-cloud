INTERFACE zif_excel_ixml_element
  PUBLIC .

  interfaces zif_excel_IXML_NODE .

  aliases APPEND_CHILD
    for zif_excel_IXML_NODE~APPEND_CHILD .
  aliases CLONE
    for zif_excel_IXML_NODE~CLONE .
  aliases CREATE_ITERATOR
    for zif_excel_IXML_NODE~CREATE_ITERATOR .
  aliases GET_ATTRIBUTES
    for zif_excel_IXML_NODE~GET_ATTRIBUTES .
  aliases GET_CHILDREN
    for zif_excel_IXML_NODE~GET_CHILDREN .
  aliases GET_FIRST_CHILD
    for zif_excel_IXML_NODE~GET_FIRST_CHILD .
  aliases GET_NAME
    for zif_excel_IXML_NODE~GET_NAME .
  aliases GET_NEXT
    for zif_excel_IXML_NODE~GET_NEXT .
  aliases GET_VALUE
    for zif_excel_IXML_NODE~GET_VALUE .
  aliases SET_VALUE
    for zif_excel_IXML_NODE~SET_VALUE .

  methods FIND_FROM_NAME
    importing
      !DEPTH type I default 0
      !NAME type STRING
      !NAMESPACE type STRING default ''
    returning
      value(RVAL) type ref to zif_excel_ixml_element .

  methods FIND_FROM_NAME_NS
    importing
      !DEPTH type I default 0
      !NAME type STRING
      !URI type STRING default ''
    returning
      value(RVAL) type ref to zif_excel_ixml_element .

  methods GET_ATTRIBUTE
    importing
      !NAME type STRING
      !NAMESPACE type STRING default ''
    returning
      value(RVAL) type STRING .

  methods GET_ATTRIBUTE_NODE_NS
    importing
      !NAME type STRING
      !URI type STRING default ''
    returning
      value(RVAL) type ref to zIF_excel_IXML_ATTRIBUTE .

  methods GET_ATTRIBUTE_NS
    importing
      !NAME type STRING
      !URI type STRING default ''
    returning
      value(RVAL) type STRING .

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

  methods REMOVE_ATTRIBUTE_NS
    importing
      !NAME type STRING
      !URI type STRING default ''
    returning
      value(RVAL) type I .

  methods SET_ATTRIBUTE
    importing
      !NAME type STRING
      !NAMESPACE type STRING default ''
      !VALUE type STRING default ''
    returning
      value(RVAL) type I .

  methods SET_ATTRIBUTE_NS
    importing
      !NAME type STRING
      !PREFIX type STRING default ''
      !URI type STRING default ''
      !VALUE type STRING default ''
    returning
      value(RVAL) type I .

ENDINTERFACE.
