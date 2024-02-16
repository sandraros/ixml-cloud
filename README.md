# ixml-cloud

**ABANDONED PROJECT / IMPORTANT INFORMATION:**
- **This current repository is a useless attempt to use SXML via the iXML interfaces, it was done thinking that iXML was not supported in ABAP Cloud.**
- **Its only interest now is to provide many iXML test classes which can be used to help understanding how iXML methods work.**

iXML exists in ABAP Cloud, the only differences with the original iXML version for ABAP not Cloud are:
- Use CL_IXML_CORE instead of CL_IXML
- All stream, parser and renderer interfaces now suffixed CORE
- The rest is unchanged (IF_IXML_* interfaces)
- For more information, see [iXML Library for ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_ixml_lib_cloud.htm)

**OLD NOTES:**
- This wrapper is currently focused on the classes and methods used by [abap2xlsx](https://github.com/abap2xlsx/abap2xlsx), and the classes are currently named with the prefixes ZCL_EXCEL_IXML\*, ZIF_EXCEL_IXML\* and ZCL_EXCEL_XML_DOCUMENT.
- In the end, the objects will be named ZCL_ISXML_\*, ZIF_ISXML_\*, etc. (`ISXML` for combining IXML and SXML).
- Steps to replace IXML with ISXML in your code:
  - Replace all occurrences of CL_IXML with ZCL_ISXML
  - Replace all occurrences of IF_IXML\* with ZIF_ISXML\*
  - Replace all occurrences of `stream_factory->create_ostream_???( dobj )` with `stream_factory->create_ostream_???( REF #( dobj ) )`
