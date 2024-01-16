INTERFACE zif_excel_ixml_node
  PUBLIC .

  methods QUERY_INTERFACE
    importing
      !IID type I
    returning
      value(RVAL) type ref to IF_IXML_UNKNOWN .

  methods APPEND_CHILD
    importing
      !NEW_CHILD type ref to zif_excel_ixml_node
    returning
      value(RVAL) type I .

  methods CLONE
    importing
      !DEPTH type I default -1
    returning
      value(RVAL) type ref to zif_excel_ixml_node .

  methods CREATE_ITERATOR
    importing
      !DEPTH type I default 0
    returning
      value(RVAL) type ref to zif_excel_IXML_NODE_ITERATOR .

  methods GET_ATTRIBUTES
    returning
      value(RVAL) type ref to zIF_excel_IXML_NAMED_NODE_MAP .

  methods GET_CHILDREN
    returning
      value(RVAL) type ref to zif_excel_IXML_NODE_LIST .

  methods GET_FIRST_CHILD
    returning
      value(RVAL) type ref to zif_excel_IXML_NODE .

  methods GET_NAME
    returning
      value(RVAL) type STRING .

  methods GET_NAMESPACE_PREFIX
    returning
      value(RVAL) type STRING .

  methods GET_NAMESPACE_URI
    returning
      value(RVAL) type STRING .

  methods GET_NEXT
    returning
      value(RVAL) type ref to zif_excel_ixml_node .

  methods GET_VALUE
    returning
      value(RVAL) type STRING .

  methods SET_NAMESPACE_PREFIX
    importing
      !PREFIX type STRING
    returning
      value(RVAL) type I .

  methods SET_VALUE
    importing
      !VALUE type STRING
    returning
      value(RVAL) type I .
ENDINTERFACE.
