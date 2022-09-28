<?xml version='1.0' encoding='UTF-8' standalone='yes' ?>
<tagfile doxygen_version="1.9.3">
  <compound kind="file">
    <name>criAlgorithm.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_algorithm_8f90.html</filename>
    <class kind="interface">crialgorithm::tostring</class>
    <class kind="interface">crialgorithm::centered</class>
    <class kind="interface">crialgorithm::lower_bound</class>
    <class kind="interface">crialgorithm::optionaldefault</class>
    <namespace>crialgorithm</namespace>
    <member kind="function">
      <type>logical function</type>
      <name>ispresent</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a933e1196b48674887b485ca7f166a805</anchor>
      <arglist>(val, list, index)</arglist>
    </member>
    <member kind="function">
      <type>character(len=len(str)) function</type>
      <name>replaceall</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>af2d693406e930e764a34f722407e4344</anchor>
      <arglist>(str, from, to)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_int</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a8ad83d20d966a26b52e9815366803828</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_real</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>ab1d975bc61d3bb162edd3db5bbb63593</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_double</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>aaae6239d94d8c7e186ad1a3a736d0afb</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>centered_int</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a90eca76b5a6a17c23e246a798e5a3841</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(val)) function</type>
      <name>centered_string</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>aa15d2f3313a7f568779c2f12ff542195</anchor>
      <arglist>(val)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>optionaldefault_logical</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a4dfa2a7c8fdb58324dd7db19bade60d3</anchor>
      <arglist>(value, default)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function</type>
      <name>optionaldefault_integer</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a90fc487c8bb954a5386f495026980871</anchor>
      <arglist>(value, default)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criConfigReader.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_config_reader_8f90.html</filename>
    <namespace>criconfigreader</namespace>
    <member kind="function">
      <type>logical function</type>
      <name>readkeyword</name>
      <anchorfile>namespacecriconfigreader.html</anchorfile>
      <anchor>a345a9fbfae8b657dc08bfc8e9f6a90a8</anchor>
      <arglist>(cnfunit, map, value)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criErrcodes.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_errcodes_8f90.html</filename>
    <namespace>crierrcodes</namespace>
    <member kind="function">
      <type>elemental logical function</type>
      <name>is_error</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a378dc4d445bbc83aa91324ddd6a31b7c</anchor>
      <arglist>(errcode)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crisuccess</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3101ebe82145a276cdf5a9d66f3bba0e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierror</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a70bd31a2088ab42350521a0d5d65f093</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crifailure</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0466488c7ffc8137abb25c01bbda1770</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_numnan</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ae881a271525111807c2b45554f16f0d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badargs</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac3f6ea596bbd669f56416cd6b574d8c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_baddims</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac90336dd9308fad6fe0017fb9e9984da</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nullptr</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5eaa50987a469e71f97c64c88f3ece4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nonalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a482f4edd1905aed29f39f0d774b90337</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badvalue</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ab6bd41c060605939b36c4699c0d6cf1b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badindex</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0c91b2c5a68b2f421c7c48558afbb7a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_mem</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5d73f9d989b1d22b0301f5219d256aef</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a474fc63c53228426301e48b95c25140a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memdealloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a186b82681f148f4ef3ca5857cd0716b4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_io</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a623ae588655ed06aacc26fb412cab57a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioopen</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac6636893d79a860ba25b7d17cc1f9333</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioread</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0452c349c8fd20c2de4d9e4b64ac3425</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iowrite</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3981d388a0ba2e4a4a846f41902c6089</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iofrmt</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a7735755e98e4391b4a4d0302e13b5795</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badtype</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a65becd6afed2e50d229f61b12e3dcdbd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badcast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac9118132667a95ed6914c5c52042d768</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_dyncast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a056bda08485cf6016cc4fffd96e45f17</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_typesel</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a8f5ceee93b61201a48435e40a8af7c43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_notimplemented</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a4e15e3a8f3ffcc95ea3af39555b6653b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crisuccess</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3101ebe82145a276cdf5a9d66f3bba0e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierror</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a70bd31a2088ab42350521a0d5d65f093</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crifailure</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0466488c7ffc8137abb25c01bbda1770</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_numnan</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ae881a271525111807c2b45554f16f0d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badargs</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac3f6ea596bbd669f56416cd6b574d8c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_baddims</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac90336dd9308fad6fe0017fb9e9984da</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nullptr</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5eaa50987a469e71f97c64c88f3ece4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nonalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a482f4edd1905aed29f39f0d774b90337</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badvalue</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ab6bd41c060605939b36c4699c0d6cf1b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badindex</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0c91b2c5a68b2f421c7c48558afbb7a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_mem</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5d73f9d989b1d22b0301f5219d256aef</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a474fc63c53228426301e48b95c25140a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memdealloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a186b82681f148f4ef3ca5857cd0716b4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_io</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a623ae588655ed06aacc26fb412cab57a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioopen</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac6636893d79a860ba25b7d17cc1f9333</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioread</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0452c349c8fd20c2de4d9e4b64ac3425</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iowrite</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3981d388a0ba2e4a4a846f41902c6089</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iofrmt</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a7735755e98e4391b4a4d0302e13b5795</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badtype</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a65becd6afed2e50d229f61b12e3dcdbd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badcast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac9118132667a95ed6914c5c52042d768</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_dyncast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a056bda08485cf6016cc4fffd96e45f17</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_typesel</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a8f5ceee93b61201a48435e40a8af7c43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_notimplemented</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a4e15e3a8f3ffcc95ea3af39555b6653b</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criLinearMap.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_linear_map_8f90.html</filename>
    <class kind="type">crilinearmap::mapitem</class>
    <namespace>crilinearmap</namespace>
    <member kind="function">
      <type>logical function</type>
      <name>resolvename</name>
      <anchorfile>namespacecrilinearmap.html</anchorfile>
      <anchor>a1970584de6e99087fa79a30f5ae036a6</anchor>
      <arglist>(themap, name, id, index)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>resolveid</name>
      <anchorfile>namespacecrilinearmap.html</anchorfile>
      <anchor>a0348dc52347bdafd3d27b0726795fc47</anchor>
      <arglist>(themap, id, name, index)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criLog.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_log_8f90.html</filename>
    <class kind="type">crilog::logdata</class>
    <class kind="interface">crilog::logit</class>
    <class kind="interface">crilog::logevent</class>
    <class kind="interface">crilog::dologging</class>
    <namespace>crilog</namespace>
    <member kind="function">
      <type>pure logical function</type>
      <name>dologging_integer</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>acc773b9ff46e9f824617f24e787d96b2</anchor>
      <arglist>(severity, refLogLevel)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>dologging_logdata</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>af12de91ea6428cf74c63bf112e61cb1e</anchor>
      <arglist>(logunit, severity)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>isloglevelok</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a4dbab0ba3b0abc5271f01a5306f79cec</anchor>
      <arglist>(severity)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilognone</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a8cb67eb38866d369b41d19ab9183c2ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogerr</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ad1367de527b8132bd9829920ce76bed1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogwarn</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ab82a11c3475d80da08329aaec59f0a00</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>criloginfo</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ac731b99e00a86db75f24ce643ecc618d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogdebug</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a2bbec29ec1e0b562d358fbc70672450c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilognone</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a8cb67eb38866d369b41d19ab9183c2ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogerr</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ad1367de527b8132bd9829920ce76bed1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogwarn</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ab82a11c3475d80da08329aaec59f0a00</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>criloginfo</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ac731b99e00a86db75f24ce643ecc618d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogdebug</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a2bbec29ec1e0b562d358fbc70672450c</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criMathUtils.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_math_utils_8f90.html</filename>
    <class kind="type">crimathutils::srtensor</class>
    <class kind="interface">crimathutils::rotatesrtensorto</class>
    <class kind="interface">crimathutils::rotatesrtensorfrom</class>
    <class kind="interface">crimathutils::ocross_product</class>
    <class kind="interface">crimathutils::vector_product</class>
    <class kind="type">crimathutils::eulerangles</class>
    <class kind="interface">crimathutils::rad2deg</class>
    <class kind="interface">crimathutils::deg2rad</class>
    <class kind="type">crimathutils::pair_double</class>
    <class kind="interface">crimathutils::rotmat</class>
    <class kind="interface">crimathutils::trace</class>
    <namespace>crimathutils</namespace>
    <member kind="function">
      <type>elemental double precision function</type>
      <name>scalarrad2deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a389226791a5b998f744b550a62ba9d8d</anchor>
      <arglist>(alpha)</arglist>
    </member>
    <member kind="function">
      <type>elemental double precision function</type>
      <name>scalardeg2rad</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a611e7a8133529b4ff5ebca3e1d527962</anchor>
      <arglist>(alpha)</arglist>
    </member>
    <member kind="function">
      <type>elemental type(eulerangles) function</type>
      <name>euleranglesrad2deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>afbc0d906734e94f5be0a3ff9d7f107b4</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>elemental type(eulerangles) function</type>
      <name>euleranglesdeg2rad</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a1ed5a4e6492e95d322712b04c14a8291</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(3)</type>
      <name>eulerangles2arr</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a394fb503bda17b6bf621bbb7397ecb27</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>pure type(eulerangles) function</type>
      <name>arr2eulerangles</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a0fbdd4166ac92e26978d057509117a5a</anchor>
      <arglist>(arr)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(5, 5)</type>
      <name>ocross_product_dp</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>af4655cec5cd506dfd57444d5ee6ab368</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function, dimension(5, 5)</type>
      <name>ocross_product_int</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab7dd1454a34ae645c8222d4bdc0f4e96</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(size(a), size(b))</type>
      <name>ocross_product_dp</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ada855c4f87904a844da750320836d716</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function, dimension(size(a), size(b))</type>
      <name>ocross_product_int</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a53f10ff699f93194bc5406944366e674</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(3)</type>
      <name>vector_product_dp</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a92645e898c8945f240fd9ba41f37f187</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>vec_angle</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab1949c5d82d3543767d4c8e171b6d04d</anchor>
      <arglist>(u, v)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>vec_cosine</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a50a987f1089170493bd8a392ba081249</anchor>
      <arglist>(u, v)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(rot_matrix_dim, rot_matrix_dim)</type>
      <name>rotmat_triplet</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a773d282ec9f1ca0ed001c983bfb7cc1b</anchor>
      <arglist>(phi1, PHI, phi2)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(rot_matrix_dim, rot_matrix_dim)</type>
      <name>rotmat_eulerangles</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8b0208f439a01392e10e97eb2b3f009d</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>pure type(eulerangles) function</type>
      <name>euleranglestype</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad7da68e48442d8f3b70e43cc9a9672d3</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>rotatesrtensorto_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a90ecca88917124ec1b177d29f6b19abb</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>rotatesrtensorfrom_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ae285197b436d5889852b4254ae4f80a8</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure type(srtensor) function</type>
      <name>rotatesrtensorto_srtensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>adf21d35e7bbf9384e5799a2c17f5b3eb</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure type(srtensor) function</type>
      <name>rotatesrtensorfrom_srtensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa4d918e6eac574deb6ffbd8e1a7c765b</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_asymm_voigt_dim)</type>
      <name>mat33tovec3</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab707d3da563a51442612de2ce12aad10</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>vec3tomat33</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>af4918aeddcff3078779b93ddbc09bb9e</anchor>
      <arglist>(vec)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>vec6tomat33</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a9cd69660eace16f013f9818f9b843287</anchor>
      <arglist>(vec)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_symm_voigt_dim)</type>
      <name>mat33tovec6</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a5bbc8c129af6a094f67fb92104373d85</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>vec9tomat33</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8b5bfaeb41852f58e3d9f9755b8bb251</anchor>
      <arglist>(vec)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_voigt_dim)</type>
      <name>mat33tovec9</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>afae40a968b4e144498673698537573b7</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>trace_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a2e3ffcb4fc4186951a786ffb6687bb02</anchor>
      <arglist>(X)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>trace_srtensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>af34cc731177198842472b2b4d5b07747</anchor>
      <arglist>(X)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>getnormalvector2d</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a08eb18077596eeb4e606682be7b3060c</anchor>
      <arglist>(A, B, length, v, beta)</arglist>
    </member>
    <member kind="function">
      <type>integer function</type>
      <name>solvequadraticpolynomial</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a0edef1d6669e63fbd6a6e812df4b7d93</anchor>
      <arglist>(a, b, c, x)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(3, 3)</type>
      <name>vec5d2tens</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a4e158616776e62e285ed385f4a85771e</anchor>
      <arglist>(v)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(5)</type>
      <name>tens2vec5d</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad4899c6afd60edbd47366bf6aee64657</anchor>
      <arglist>(t)</arglist>
    </member>
    <member kind="variable">
      <type>type(srtensor), parameter</type>
      <name>unit_sr_tensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a85ddeba48fbdd4eb42641566db66f5bd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6a9b6fc89d6bee5567075f0b2064291f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad4918160e8df6c96122fbd76c0ba4869</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi_deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ade114c23d014db90bc9c439c7b128086</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>deg_pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab5753624d4216a85e366f556ba3c70cc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a398ac1586197cb4acfe8898840b0f86f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2i</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a2c2e1af846fb820b31cf71e4a9462c06</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root23</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8f5008cdecddc5422b823e7849e655d8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root32</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>adfb13757cc089db703babbe44cc9053f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_tensor_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a4c8bab38c29f02139ed7d3bfba642864</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>rot_matrix_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a90f99787840fe40d0bd3cd53f0fa1755</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_asymm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6b9e7a537da6ffeea6434748617dd48</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_symm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6f6ba7d874dd79d42873b2026b4d62c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a15d1e2e661ee56108c9518fd3864ff69</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(sr_tensor_dim, sr_tensor_dim), parameter</type>
      <name>unit_sr_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6149c01cf6f4e5850a34a8823324c8a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6a9b6fc89d6bee5567075f0b2064291f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad4918160e8df6c96122fbd76c0ba4869</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi_deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ade114c23d014db90bc9c439c7b128086</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>deg_pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab5753624d4216a85e366f556ba3c70cc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a398ac1586197cb4acfe8898840b0f86f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2i</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a2c2e1af846fb820b31cf71e4a9462c06</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root23</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8f5008cdecddc5422b823e7849e655d8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root32</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>adfb13757cc089db703babbe44cc9053f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_tensor_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a4c8bab38c29f02139ed7d3bfba642864</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>rot_matrix_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a90f99787840fe40d0bd3cd53f0fa1755</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_asymm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6b9e7a537da6ffeea6434748617dd48</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_symm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6f6ba7d874dd79d42873b2026b4d62c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a15d1e2e661ee56108c9518fd3864ff69</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(sr_tensor_dim, sr_tensor_dim), parameter</type>
      <name>unit_sr_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6149c01cf6f4e5850a34a8823324c8a0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criNamedRange.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_named_range_8f90.html</filename>
    <namespace>crinamedrange</namespace>
    <member kind="function">
      <type>class(range_type) function, pointer</type>
      <name>rangefactory</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a3eec7f47f4e78ed876939595d932c5cc</anchor>
      <arglist>(name)</arglist>
    </member>
    <member kind="function">
      <type>class(range_type) function, pointer</type>
      <name>rangefactory_extended</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a9c861a40683d02d87ca6847c83a5f7d6</anchor>
      <arglist>(name)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_uniform_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a34bb75e452f7cea5ff562504297f0121</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_biased_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a3d6c8f53a211ec2155d07eaa43a95fb1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_doublebiased_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>aa96429c1133a5157b4997255f32ca949</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_multibiased_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a8926f08c0b74156b94bc36220ba88f53</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_discrete_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a9424d32a3e4b4d1684c0e11a0db96c64</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(mapitem), dimension(range_ntypes), parameter</type>
      <name>range_name_map</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a504a00b38f22af4f39b2938012c88248</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(mapitem), dimension(range_nextensions), parameter</type>
      <name>range_name_extensions_map</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a87fd5db56a963e6794920aa841c6290a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_zero_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>aaa37f71d4dcfa06a392724bf7f0f2147</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_one_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a5e66c3741e45a8f55661c2279d7bceda</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_zero_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>aaa37f71d4dcfa06a392724bf7f0f2147</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_one_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a5e66c3741e45a8f55661c2279d7bceda</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criNumerics.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_numerics_8f90.html</filename>
    <class kind="interface">crinumerics::linspace</class>
    <class kind="type">crinumerics::barycentricinterpolator</class>
    <class kind="interface">crinumerics::interpolate</class>
    <namespace>crinumerics</namespace>
    <member kind="function">
      <type>pure subroutine</type>
      <name>linspace_dynarr</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>ad7b8f7f9ef7f8735bfa84334af363dda</anchor>
      <arglist>(xstart, xend, n, array, endpoint)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>linspace_arr</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a560d856b796afe26e70ae52a731c53e3</anchor>
      <arglist>(xstart, xend, array, endpoint)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>barycentricinterpolator_init</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a66099be4de96c83c6a4eb0a90b5dd53e</anchor>
      <arglist>(this, order, xi, yi, info)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>barycentricinterpolator_init_allocate</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>aaae28aeffc6f4341ce08105fdf319493</anchor>
      <arglist>(this, order, npoints, info)</arglist>
    </member>
    <member kind="function">
      <type>double precision pure function</type>
      <name>barycentricinterpolator_interpolate</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a59cea91da1ef4c174d1a1a13e936f0e6</anchor>
      <arglist>(this, x)</arglist>
    </member>
    <member kind="function">
      <type>double precision pure function</type>
      <name>barycentric_interpolation</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a6d5c29ffcf15b950a6b16b9fe73cf45c</anchor>
      <arglist>(x, xi, yi, wi)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>barycentric_weights</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>ab41904a084be454ee5d8cafdbd7ec832</anchor>
      <arglist>(xi, wi, info)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criPath.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_path_8f90.html</filename>
    <namespace>cripath</namespace>
    <member kind="function">
      <type>pure character(len=len(path)) function</type>
      <name>basename</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>acf33272936fc9b3fb2a79f303d90b613</anchor>
      <arglist>(path)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(path)) function</type>
      <name>stripext</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>ab6284d364899aeac072beaf949182ea6</anchor>
      <arglist>(path)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>splitext</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a427e721cd0af1d4810238292fb521fc4</anchor>
      <arglist>(path, root, ext)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(prefix)+len(suffix)) function</type>
      <name>mkfilename</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a59882556ce377835f391465127e8120c</anchor>
      <arglist>(prefix, suffix)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(path_a)+len(path_b)) function</type>
      <name>pathjoin</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>aaf9b652b0fd1d069cbfab97daa69221e</anchor>
      <arglist>(path_a, path_b)</arglist>
    </member>
    <member kind="variable">
      <type>character, parameter</type>
      <name>pathsep</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a3b212c2ebc04a77130a7b47259cb431f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>max_pathlen</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a431565de6cbae010916555dd42fb8f5e</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criRange.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_range_8f90.html</filename>
    <namespace>crirange</namespace>
  </compound>
  <compound kind="file">
    <name>criRuntime.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_runtime_8f90.html</filename>
    <class kind="type">criruntime::commandline</class>
    <namespace>criruntime</namespace>
    <member kind="function">
      <type>subroutine</type>
      <name>finalize</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a63427cf84230e038e9283c85459955f2</anchor>
      <arglist>(errcode)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>processcommandline</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aee8edd047b0ba1d5a5f0067ffacfdf6c</anchor>
      <arglist>(this, argc_min, argc_max, command_map, command_argpos, info, terminate)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>finishprocessing</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>afa1548197c8e0c3d4b589cb7effa1e74</anchor>
      <arglist>(this, command_map, info, terminate)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>printhelpmessage</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>ae4940aef753d2b1322a27659dc0b8ade</anchor>
      <arglist>(this, command_map, info)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>getargv</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a7f6bc1a11847d6ec29643785f832ca12</anchor>
      <arglist>(argc, argv, info)</arglist>
    </member>
    <member kind="function">
      <type>integer function</type>
      <name>openordie</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>ae24680de8b2573d1d72ce20f42a9ef2d</anchor>
      <arglist>(fpath, status)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>errmsg_len</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aadf126359318d1657fa7dc4c80a2a053</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character(len=errmsg_len), save</type>
      <name>errmsg</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a6be05f9853d153a53e3f1511df7134ed</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>description_len</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>affe08f177754552f44d45bbb95a1c128</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>max_command_param_len</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>acf96fd012797ef564d36c611757c3adb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ok</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a4b23d17de3a3852fe7b1d26931cde693</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_inputerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aa35f23bf5c0993afba7de5a8f6b975d2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ioerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a35bbce4dcd8dfca411591b89ae418d9d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_runtimeerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a17e4716ebdaf69e8e87f7ca09b9325ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ok</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a4b23d17de3a3852fe7b1d26931cde693</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_inputerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aa35f23bf5c0993afba7de5a8f6b975d2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ioerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a35bbce4dcd8dfca411591b89ae418d9d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_runtimeerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a17e4716ebdaf69e8e87f7ca09b9325ab</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criTest.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_test_8f90.html</filename>
    <namespace>critest</namespace>
    <member kind="function">
      <type>subroutine, public</type>
      <name>testinit</name>
      <anchorfile>namespacecritest.html</anchorfile>
      <anchor>a5898f57d07c3ec16d6a299c676da8fbd</anchor>
      <arglist>(outunit)</arglist>
    </member>
    <member kind="function">
      <type>subroutine, public</type>
      <name>testsummary</name>
      <anchorfile>namespacecritest.html</anchorfile>
      <anchor>ac2a8e18b5e67c8331d64109719b0a997</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>subroutine, public</type>
      <name>testreport</name>
      <anchorfile>namespacecritest.html</anchorfile>
      <anchor>a72e58e58d57e4159f4e353fdd6f84688</anchor>
      <arglist>(name, outcome, line, file)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>criUncomment.f90</name>
    <path>/home/stijn/TWRMTMProject/FCRI/src/</path>
    <filename>cri_uncomment_8f90.html</filename>
    <class kind="interface">criuncomment::readvalue</class>
    <namespace>criuncomment</namespace>
    <member kind="function">
      <type>pure subroutine</type>
      <name>stripcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a6c550f9bbf2f78d61b11e833be9cd479</anchor>
      <arglist>(line, comment_mark)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>skipcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a5b192b07be757c83a4cb818aa133cd85</anchor>
      <arglist>(nunit, buffer)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>iscomment</name>
      <anchorfile>cri_uncomment_8f90.html</anchorfile>
      <anchor>abc1c959287b666fee63ea4fcce3f240d</anchor>
      <arglist>(buffer)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>max_line_len</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a54e44a42a767eb45f828b3328af1a82c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character, parameter</type>
      <name>comment_sign</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a045aed2f669fb3efaa51564c32ad7abd</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>stripcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a6c550f9bbf2f78d61b11e833be9cd479</anchor>
      <arglist>(line, comment_mark)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>skipcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a5b192b07be757c83a4cb818aa133cd85</anchor>
      <arglist>(nunit, buffer)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>iscomment</name>
      <anchorfile>cri_uncomment_8f90.html</anchorfile>
      <anchor>abc1c959287b666fee63ea4fcce3f240d</anchor>
      <arglist>(buffer)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>crinumerics::barycentricinterpolator</name>
    <filename>structcrinumerics_1_1barycentricinterpolator.html</filename>
    <member kind="variable">
      <type>integer</type>
      <name>order</name>
      <anchorfile>structcrinumerics_1_1barycentricinterpolator.html</anchorfile>
      <anchor>a99f5f7dee6e4a753e4e73d82c7b3f6be</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>npoints</name>
      <anchorfile>structcrinumerics_1_1barycentricinterpolator.html</anchorfile>
      <anchor>a5f7ad39e31c346077b4ab3a80f0193f6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(:), allocatable</type>
      <name>xi</name>
      <anchorfile>structcrinumerics_1_1barycentricinterpolator.html</anchorfile>
      <anchor>ae4588789089443bbb3c33b82dda784fa</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(:), allocatable</type>
      <name>yi</name>
      <anchorfile>structcrinumerics_1_1barycentricinterpolator.html</anchorfile>
      <anchor>a38f82fe22ea8616f85ecfa9ed2a9d764</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(:,:), allocatable</type>
      <name>wi</name>
      <anchorfile>structcrinumerics_1_1barycentricinterpolator.html</anchorfile>
      <anchor>ab405f640b2475f4ad59f4b2e1430bb3a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>logical</type>
      <name>extrapolate</name>
      <anchorfile>structcrinumerics_1_1barycentricinterpolator.html</anchorfile>
      <anchor>a62cb6a1b49b3e51516690bb588e68ba6</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crialgorithm::centered</name>
    <filename>interfacecrialgorithm_1_1centered.html</filename>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>centered_int</name>
      <anchorfile>interfacecrialgorithm_1_1centered.html</anchorfile>
      <anchor>aa98bfe7957c1ab2b0e3eb43fc388900f</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(val)) function</type>
      <name>centered_string</name>
      <anchorfile>interfacecrialgorithm_1_1centered.html</anchorfile>
      <anchor>aa97ae529531349e0aba3f8c8831be1a7</anchor>
      <arglist>(val)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>criruntime::commandline</name>
    <filename>structcriruntime_1_1commandline.html</filename>
    <member kind="variable">
      <type>character(len=description_len)</type>
      <name>progname</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>a507866a6c05ad412b8a5e666d58a3eeb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character(len=description_len)</type>
      <name>description</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>a8f2e039ca6506957d7188fc4d59c23e2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>logical</type>
      <name>is_initialized</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>a1bcd75246f9a047aa62efcf717a54834</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>argc</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>a57a0c133d8790d0d46b9c4a915df0c16</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character(len=:), dimension(:), allocatable</type>
      <name>argv</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>aceed672917c0977520a1cd727603bd57</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>argc_opt</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>a4cdfb20c4d947a3ff75e6548136a8c81</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character(len=:), allocatable</type>
      <name>args_opt</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>a3634fc5d148c7591dcb5cf7a09b66976</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>logical</type>
      <name>is_command_identified</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>abedb96f58badc623c377f2572fd6ee02</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>command_id</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>aff121f4020cdd5ced6855bcd7dacb38a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>command_idx</name>
      <anchorfile>structcriruntime_1_1commandline.html</anchorfile>
      <anchor>adb0dabfb8c3ebc2fb87d1b2346535d54</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::deg2rad</name>
    <filename>interfacecrimathutils_1_1deg2rad.html</filename>
    <member kind="function">
      <type>elemental double precision function</type>
      <name>scalardeg2rad</name>
      <anchorfile>interfacecrimathutils_1_1deg2rad.html</anchorfile>
      <anchor>a3b2b919273bd1bb01cb251f8b3010f3d</anchor>
      <arglist>(alpha)</arglist>
    </member>
    <member kind="function">
      <type>elemental type(eulerangles) function</type>
      <name>euleranglesdeg2rad</name>
      <anchorfile>interfacecrimathutils_1_1deg2rad.html</anchorfile>
      <anchor>aaf69c080a0ed42b9a1066223d80bce64</anchor>
      <arglist>(ang)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crilog::dologging</name>
    <filename>interfacecrilog_1_1dologging.html</filename>
    <member kind="function">
      <type>pure logical function</type>
      <name>dologging_integer</name>
      <anchorfile>interfacecrilog_1_1dologging.html</anchorfile>
      <anchor>aa0d5388f40bd915c6956c950de96862c</anchor>
      <arglist>(severity, refLogLevel)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>dologging_logdata</name>
      <anchorfile>interfacecrilog_1_1dologging.html</anchorfile>
      <anchor>aa75a4316d80a151c48abcb092e56c3f5</anchor>
      <arglist>(logunit, severity)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>crimathutils::eulerangles</name>
    <filename>structcrimathutils_1_1eulerangles.html</filename>
    <member kind="variable">
      <type>double precision</type>
      <name>fi1</name>
      <anchorfile>structcrimathutils_1_1eulerangles.html</anchorfile>
      <anchor>a1800b39d648e61a2c34dff10e0158284</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision</type>
      <name>phi</name>
      <anchorfile>structcrimathutils_1_1eulerangles.html</anchorfile>
      <anchor>a05a1a5bce133aea578648f76507f2328</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision</type>
      <name>fi2</name>
      <anchorfile>structcrimathutils_1_1eulerangles.html</anchorfile>
      <anchor>a31345bcec48a307d54b448ae5b788182</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crinumerics::interpolate</name>
    <filename>interfacecrinumerics_1_1interpolate.html</filename>
    <member kind="function">
      <type>double precision pure function</type>
      <name>barycentricinterpolator_interpolate</name>
      <anchorfile>interfacecrinumerics_1_1interpolate.html</anchorfile>
      <anchor>ad8c6ff82fa2da75b08f02d64213ec4a7</anchor>
      <arglist>(this, x)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crinumerics::linspace</name>
    <filename>interfacecrinumerics_1_1linspace.html</filename>
    <member kind="function">
      <type>pure subroutine</type>
      <name>linspace_arr</name>
      <anchorfile>interfacecrinumerics_1_1linspace.html</anchorfile>
      <anchor>aee76e1508a589f7fafe4e9248a56ebff</anchor>
      <arglist>(xstart, xend, array, endpoint)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>linspace_dynarr</name>
      <anchorfile>interfacecrinumerics_1_1linspace.html</anchorfile>
      <anchor>a75d8dc8124521017c6c0dc420894cd58</anchor>
      <arglist>(xstart, xend, n, array, endpoint)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>crilog::logdata</name>
    <filename>structcrilog_1_1logdata.html</filename>
    <member kind="variable">
      <type>integer</type>
      <name>level</name>
      <anchorfile>structcrilog_1_1logdata.html</anchorfile>
      <anchor>a62eb6ac9ab219f8b8f2eb151b8fa4178</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>ounit</name>
      <anchorfile>structcrilog_1_1logdata.html</anchorfile>
      <anchor>affff8e4aa2ecbc4faf7638e4753c444f</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crilog::logevent</name>
    <filename>interfacecrilog_1_1logevent.html</filename>
    <member kind="function">
      <type></type>
      <name>logevent_string</name>
      <anchorfile>interfacecrilog_1_1logevent.html</anchorfile>
      <anchor>ada9f2b9c5be01e8075fd8f773124a5d9</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>logevent_integer</name>
      <anchorfile>interfacecrilog_1_1logevent.html</anchorfile>
      <anchor>a9f1b8c364da11e96e760c30a8a948c8e</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>logevent_logical</name>
      <anchorfile>interfacecrilog_1_1logevent.html</anchorfile>
      <anchor>ae586dd260407aae30ed6bca2e01b4991</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>logevent_double</name>
      <anchorfile>interfacecrilog_1_1logevent.html</anchorfile>
      <anchor>a876bf5fbdae77b2e90388b2bb238cefa</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crilog::logit</name>
    <filename>interfacecrilog_1_1logit.html</filename>
    <member kind="function">
      <type></type>
      <name>log_string</name>
      <anchorfile>interfacecrilog_1_1logit.html</anchorfile>
      <anchor>a3b69b01bad648a2cb033a44725c75eeb</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>log_integer</name>
      <anchorfile>interfacecrilog_1_1logit.html</anchorfile>
      <anchor>ae7fa054a5202bcbd56836c849e951c86</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>log_logical</name>
      <anchorfile>interfacecrilog_1_1logit.html</anchorfile>
      <anchor>a689ca0267231b69b645633bde7911d09</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>log_double</name>
      <anchorfile>interfacecrilog_1_1logit.html</anchorfile>
      <anchor>abd9d90c99b3703518e8ac83a09f8e78d</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crialgorithm::lower_bound</name>
    <filename>interfacecrialgorithm_1_1lower__bound.html</filename>
    <member kind="function">
      <type></type>
      <name>lower_bound_int</name>
      <anchorfile>interfacecrialgorithm_1_1lower__bound.html</anchorfile>
      <anchor>a309af1342478f081fc75a930dc331358</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>lower_bound_double</name>
      <anchorfile>interfacecrialgorithm_1_1lower__bound.html</anchorfile>
      <anchor>aad7e9d3385262702d4cb1e750741e471</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>crilinearmap::mapitem</name>
    <filename>structcrilinearmap_1_1mapitem.html</filename>
    <member kind="variable">
      <type>character(len=cmapnamelen)</type>
      <name>name</name>
      <anchorfile>structcrilinearmap_1_1mapitem.html</anchorfile>
      <anchor>a2718d6403c1801b34d8806ec4f5b51ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer</type>
      <name>id</name>
      <anchorfile>structcrilinearmap_1_1mapitem.html</anchorfile>
      <anchor>a417ed9f6ce74ceace891125b04912a1c</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::ocross_product</name>
    <filename>interfacecrimathutils_1_1ocross__product.html</filename>
    <member kind="function">
      <type>pure double precision function, dimension(size(a), size(b))</type>
      <name>ocross_product_dp</name>
      <anchorfile>interfacecrimathutils_1_1ocross__product.html</anchorfile>
      <anchor>a6e9319c78dde6db8363c075b4eacaf4e</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function, dimension(size(a), size(b))</type>
      <name>ocross_product_int</name>
      <anchorfile>interfacecrimathutils_1_1ocross__product.html</anchorfile>
      <anchor>a48d76da89acce38104cb4e7892d75e16</anchor>
      <arglist>(a, b)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crialgorithm::optionaldefault</name>
    <filename>interfacecrialgorithm_1_1optionaldefault.html</filename>
    <member kind="function">
      <type>pure logical function</type>
      <name>optionaldefault_logical</name>
      <anchorfile>interfacecrialgorithm_1_1optionaldefault.html</anchorfile>
      <anchor>a75210660459101c790f1e1377f56ed94</anchor>
      <arglist>(value, default)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function</type>
      <name>optionaldefault_integer</name>
      <anchorfile>interfacecrialgorithm_1_1optionaldefault.html</anchorfile>
      <anchor>a088969c79fab4c0dc7019f9edd2a10d9</anchor>
      <arglist>(value, default)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>crimathutils::pair_double</name>
    <filename>structcrimathutils_1_1pair__double.html</filename>
    <member kind="variable">
      <type>double precision</type>
      <name>x</name>
      <anchorfile>structcrimathutils_1_1pair__double.html</anchorfile>
      <anchor>af8c0e7e9ba545cc872882d5aa90b152f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision</type>
      <name>y</name>
      <anchorfile>structcrimathutils_1_1pair__double.html</anchorfile>
      <anchor>a0ff482a51e7f92ed53879ec575528657</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::rad2deg</name>
    <filename>interfacecrimathutils_1_1rad2deg.html</filename>
    <member kind="function">
      <type>elemental double precision function</type>
      <name>scalarrad2deg</name>
      <anchorfile>interfacecrimathutils_1_1rad2deg.html</anchorfile>
      <anchor>acd90673bc22d67a5a650795ecaff54ac</anchor>
      <arglist>(alpha)</arglist>
    </member>
    <member kind="function">
      <type>elemental type(eulerangles) function</type>
      <name>euleranglesrad2deg</name>
      <anchorfile>interfacecrimathutils_1_1rad2deg.html</anchorfile>
      <anchor>a63a3959f339d7c0f1bb56b3b909148a5</anchor>
      <arglist>(ang)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>criuncomment::readvalue</name>
    <filename>interfacecriuncomment_1_1readvalue.html</filename>
    <member kind="function">
      <type></type>
      <name>read_integer</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>a802b081c17a5745a7e6a2088adf72b27</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_vector_integer</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>af4fadc85be0542ca28a0f7a06294f6d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_logical</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>ac3c249f622d37efcc6936a23e0e6277c</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_vector_logical</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>ae89e8b3552f9ecfdfec568e2d5466a6b</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_string</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>a09c992fdbcec8e70db4f71913109b990</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_vector_string</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>ae0856279fe104f50787c036d2aeacd29</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_double</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>a9ef059762b065eb1aba358beeb6840e8</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>read_vector_double</name>
      <anchorfile>interfacecriuncomment_1_1readvalue.html</anchorfile>
      <anchor>a80752928b399e418fb037d677e988f8b</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::rotatesrtensorfrom</name>
    <filename>interfacecrimathutils_1_1rotatesrtensorfrom.html</filename>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>rotatesrtensorfrom_matrix</name>
      <anchorfile>interfacecrimathutils_1_1rotatesrtensorfrom.html</anchorfile>
      <anchor>a7961b6633fc39becceef48159bdd93b8</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure type(srtensor) function</type>
      <name>rotatesrtensorfrom_srtensor</name>
      <anchorfile>interfacecrimathutils_1_1rotatesrtensorfrom.html</anchorfile>
      <anchor>a78d3edfd0f8ec5a8b9a8fa884845e66a</anchor>
      <arglist>(S, R)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::rotatesrtensorto</name>
    <filename>interfacecrimathutils_1_1rotatesrtensorto.html</filename>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>rotatesrtensorto_matrix</name>
      <anchorfile>interfacecrimathutils_1_1rotatesrtensorto.html</anchorfile>
      <anchor>a66320748b5c0d9b0f2623e93e8f73fc3</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure type(srtensor) function</type>
      <name>rotatesrtensorto_srtensor</name>
      <anchorfile>interfacecrimathutils_1_1rotatesrtensorto.html</anchorfile>
      <anchor>ab9bfd24ffa77dfbebfc63d5a349824cb</anchor>
      <arglist>(S, R)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::rotmat</name>
    <filename>interfacecrimathutils_1_1rotmat.html</filename>
    <member kind="function">
      <type>pure double precision function, dimension(rot_matrix_dim, rot_matrix_dim)</type>
      <name>rotmat_triplet</name>
      <anchorfile>interfacecrimathutils_1_1rotmat.html</anchorfile>
      <anchor>ac8e96c59dce7507a864aeef8b71b3fbd</anchor>
      <arglist>(phi1, PHI, phi2)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(rot_matrix_dim, rot_matrix_dim)</type>
      <name>rotmat_eulerangles</name>
      <anchorfile>interfacecrimathutils_1_1rotmat.html</anchorfile>
      <anchor>a8a74c4dddecdb4b50b8627d48eb8eb8e</anchor>
      <arglist>(ang)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>crimathutils::srtensor</name>
    <filename>structcrimathutils_1_1srtensor.html</filename>
    <member kind="variable">
      <type>double precision, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>t</name>
      <anchorfile>structcrimathutils_1_1srtensor.html</anchorfile>
      <anchor>af7465ba700a40f4354910aff30e243e6</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crialgorithm::tostring</name>
    <filename>interfacecrialgorithm_1_1tostring.html</filename>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_int</name>
      <anchorfile>interfacecrialgorithm_1_1tostring.html</anchorfile>
      <anchor>ae0e4022398a8aeaae6a76981cf33ef9b</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_real</name>
      <anchorfile>interfacecrialgorithm_1_1tostring.html</anchorfile>
      <anchor>a06e57d57e22e5393b0c3297db600512f</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_double</name>
      <anchorfile>interfacecrialgorithm_1_1tostring.html</anchorfile>
      <anchor>ac32260cc91d2c1a06818e2f66f53d16e</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::trace</name>
    <filename>interfacecrimathutils_1_1trace.html</filename>
    <member kind="function">
      <type>pure double precision function</type>
      <name>trace_matrix</name>
      <anchorfile>interfacecrimathutils_1_1trace.html</anchorfile>
      <anchor>a1ee3a7206763ccf9c13752c6c686284e</anchor>
      <arglist>(X)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>trace_srtensor</name>
      <anchorfile>interfacecrimathutils_1_1trace.html</anchorfile>
      <anchor>a8b25789e19dac6f453792da1053814fc</anchor>
      <arglist>(X)</arglist>
    </member>
  </compound>
  <compound kind="interface">
    <name>crimathutils::vector_product</name>
    <filename>interfacecrimathutils_1_1vector__product.html</filename>
    <member kind="function">
      <type>pure double precision function, dimension(3)</type>
      <name>vector_product_dp</name>
      <anchorfile>interfacecrimathutils_1_1vector__product.html</anchorfile>
      <anchor>a9ee8f6808376753a4b52138377b0d4d6</anchor>
      <arglist>(a, b)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crialgorithm</name>
    <filename>namespacecrialgorithm.html</filename>
    <class kind="interface">crialgorithm::centered</class>
    <class kind="interface">crialgorithm::lower_bound</class>
    <class kind="interface">crialgorithm::optionaldefault</class>
    <class kind="interface">crialgorithm::tostring</class>
    <member kind="function">
      <type>logical function</type>
      <name>ispresent</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a933e1196b48674887b485ca7f166a805</anchor>
      <arglist>(val, list, index)</arglist>
    </member>
    <member kind="function">
      <type>character(len=len(str)) function</type>
      <name>replaceall</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>af2d693406e930e764a34f722407e4344</anchor>
      <arglist>(str, from, to)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_int</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a8ad83d20d966a26b52e9815366803828</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_real</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>ab1d975bc61d3bb162edd3db5bbb63593</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>tostring_double</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>aaae6239d94d8c7e186ad1a3a736d0afb</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=strlen) function</type>
      <name>centered_int</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a90eca76b5a6a17c23e246a798e5a3841</anchor>
      <arglist>(val, strlen, fmt)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(val)) function</type>
      <name>centered_string</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>aa15d2f3313a7f568779c2f12ff542195</anchor>
      <arglist>(val)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>optionaldefault_logical</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a4dfa2a7c8fdb58324dd7db19bade60d3</anchor>
      <arglist>(value, default)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function</type>
      <name>optionaldefault_integer</name>
      <anchorfile>namespacecrialgorithm.html</anchorfile>
      <anchor>a90fc487c8bb954a5386f495026980871</anchor>
      <arglist>(value, default)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>criconfigreader</name>
    <filename>namespacecriconfigreader.html</filename>
    <member kind="function">
      <type>logical function</type>
      <name>readkeyword</name>
      <anchorfile>namespacecriconfigreader.html</anchorfile>
      <anchor>a345a9fbfae8b657dc08bfc8e9f6a90a8</anchor>
      <arglist>(cnfunit, map, value)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crierrcodes</name>
    <filename>namespacecrierrcodes.html</filename>
    <member kind="function">
      <type>elemental logical function</type>
      <name>is_error</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a378dc4d445bbc83aa91324ddd6a31b7c</anchor>
      <arglist>(errcode)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crisuccess</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3101ebe82145a276cdf5a9d66f3bba0e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierror</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a70bd31a2088ab42350521a0d5d65f093</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crifailure</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0466488c7ffc8137abb25c01bbda1770</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_numnan</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ae881a271525111807c2b45554f16f0d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badargs</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac3f6ea596bbd669f56416cd6b574d8c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_baddims</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac90336dd9308fad6fe0017fb9e9984da</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nullptr</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5eaa50987a469e71f97c64c88f3ece4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nonalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a482f4edd1905aed29f39f0d774b90337</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badvalue</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ab6bd41c060605939b36c4699c0d6cf1b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badindex</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0c91b2c5a68b2f421c7c48558afbb7a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_mem</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5d73f9d989b1d22b0301f5219d256aef</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a474fc63c53228426301e48b95c25140a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memdealloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a186b82681f148f4ef3ca5857cd0716b4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_io</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a623ae588655ed06aacc26fb412cab57a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioopen</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac6636893d79a860ba25b7d17cc1f9333</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioread</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0452c349c8fd20c2de4d9e4b64ac3425</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iowrite</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3981d388a0ba2e4a4a846f41902c6089</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iofrmt</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a7735755e98e4391b4a4d0302e13b5795</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badtype</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a65becd6afed2e50d229f61b12e3dcdbd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badcast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac9118132667a95ed6914c5c52042d768</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_dyncast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a056bda08485cf6016cc4fffd96e45f17</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_typesel</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a8f5ceee93b61201a48435e40a8af7c43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_notimplemented</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a4e15e3a8f3ffcc95ea3af39555b6653b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crisuccess</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3101ebe82145a276cdf5a9d66f3bba0e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierror</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a70bd31a2088ab42350521a0d5d65f093</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crifailure</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0466488c7ffc8137abb25c01bbda1770</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_numnan</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ae881a271525111807c2b45554f16f0d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badargs</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac3f6ea596bbd669f56416cd6b574d8c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_baddims</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac90336dd9308fad6fe0017fb9e9984da</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nullptr</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5eaa50987a469e71f97c64c88f3ece4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_nonalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a482f4edd1905aed29f39f0d774b90337</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badvalue</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ab6bd41c060605939b36c4699c0d6cf1b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badindex</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0c91b2c5a68b2f421c7c48558afbb7a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_mem</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a5d73f9d989b1d22b0301f5219d256aef</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memalloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a474fc63c53228426301e48b95c25140a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_memdealloc</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a186b82681f148f4ef3ca5857cd0716b4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_io</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a623ae588655ed06aacc26fb412cab57a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioopen</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac6636893d79a860ba25b7d17cc1f9333</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_ioread</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a0452c349c8fd20c2de4d9e4b64ac3425</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iowrite</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a3981d388a0ba2e4a4a846f41902c6089</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_iofrmt</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a7735755e98e4391b4a4d0302e13b5795</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badtype</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a65becd6afed2e50d229f61b12e3dcdbd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_badcast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>ac9118132667a95ed6914c5c52042d768</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_dyncast</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a056bda08485cf6016cc4fffd96e45f17</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_typesel</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a8f5ceee93b61201a48435e40a8af7c43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crierr_notimplemented</name>
      <anchorfile>namespacecrierrcodes.html</anchorfile>
      <anchor>a4e15e3a8f3ffcc95ea3af39555b6653b</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crilinearmap</name>
    <filename>namespacecrilinearmap.html</filename>
    <class kind="type">crilinearmap::mapitem</class>
    <member kind="function">
      <type>logical function</type>
      <name>resolvename</name>
      <anchorfile>namespacecrilinearmap.html</anchorfile>
      <anchor>a1970584de6e99087fa79a30f5ae036a6</anchor>
      <arglist>(themap, name, id, index)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>resolveid</name>
      <anchorfile>namespacecrilinearmap.html</anchorfile>
      <anchor>a0348dc52347bdafd3d27b0726795fc47</anchor>
      <arglist>(themap, id, name, index)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crilog</name>
    <filename>namespacecrilog.html</filename>
    <class kind="interface">crilog::dologging</class>
    <class kind="type">crilog::logdata</class>
    <class kind="interface">crilog::logevent</class>
    <class kind="interface">crilog::logit</class>
    <member kind="function">
      <type>pure logical function</type>
      <name>dologging_integer</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>acc773b9ff46e9f824617f24e787d96b2</anchor>
      <arglist>(severity, refLogLevel)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>dologging_logdata</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>af12de91ea6428cf74c63bf112e61cb1e</anchor>
      <arglist>(logunit, severity)</arglist>
    </member>
    <member kind="function">
      <type>pure logical function</type>
      <name>isloglevelok</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a4dbab0ba3b0abc5271f01a5306f79cec</anchor>
      <arglist>(severity)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilognone</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a8cb67eb38866d369b41d19ab9183c2ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogerr</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ad1367de527b8132bd9829920ce76bed1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogwarn</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ab82a11c3475d80da08329aaec59f0a00</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>criloginfo</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ac731b99e00a86db75f24ce643ecc618d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogdebug</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a2bbec29ec1e0b562d358fbc70672450c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilognone</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a8cb67eb38866d369b41d19ab9183c2ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogerr</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ad1367de527b8132bd9829920ce76bed1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogwarn</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ab82a11c3475d80da08329aaec59f0a00</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>criloginfo</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>ac731b99e00a86db75f24ce643ecc618d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>crilogdebug</name>
      <anchorfile>namespacecrilog.html</anchorfile>
      <anchor>a2bbec29ec1e0b562d358fbc70672450c</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crimathutils</name>
    <filename>namespacecrimathutils.html</filename>
    <class kind="interface">crimathutils::deg2rad</class>
    <class kind="type">crimathutils::eulerangles</class>
    <class kind="interface">crimathutils::ocross_product</class>
    <class kind="type">crimathutils::pair_double</class>
    <class kind="interface">crimathutils::rad2deg</class>
    <class kind="interface">crimathutils::rotatesrtensorfrom</class>
    <class kind="interface">crimathutils::rotatesrtensorto</class>
    <class kind="interface">crimathutils::rotmat</class>
    <class kind="type">crimathutils::srtensor</class>
    <class kind="interface">crimathutils::trace</class>
    <class kind="interface">crimathutils::vector_product</class>
    <member kind="function">
      <type>elemental double precision function</type>
      <name>scalarrad2deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a389226791a5b998f744b550a62ba9d8d</anchor>
      <arglist>(alpha)</arglist>
    </member>
    <member kind="function">
      <type>elemental double precision function</type>
      <name>scalardeg2rad</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a611e7a8133529b4ff5ebca3e1d527962</anchor>
      <arglist>(alpha)</arglist>
    </member>
    <member kind="function">
      <type>elemental type(eulerangles) function</type>
      <name>euleranglesrad2deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>afbc0d906734e94f5be0a3ff9d7f107b4</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>elemental type(eulerangles) function</type>
      <name>euleranglesdeg2rad</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a1ed5a4e6492e95d322712b04c14a8291</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(3)</type>
      <name>eulerangles2arr</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a394fb503bda17b6bf621bbb7397ecb27</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>pure type(eulerangles) function</type>
      <name>arr2eulerangles</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a0fbdd4166ac92e26978d057509117a5a</anchor>
      <arglist>(arr)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(5, 5)</type>
      <name>ocross_product_dp</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>af4655cec5cd506dfd57444d5ee6ab368</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function, dimension(5, 5)</type>
      <name>ocross_product_int</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab7dd1454a34ae645c8222d4bdc0f4e96</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(size(a), size(b))</type>
      <name>ocross_product_dp</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ada855c4f87904a844da750320836d716</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure integer function, dimension(size(a), size(b))</type>
      <name>ocross_product_int</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a53f10ff699f93194bc5406944366e674</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(3)</type>
      <name>vector_product_dp</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a92645e898c8945f240fd9ba41f37f187</anchor>
      <arglist>(a, b)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>vec_angle</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab1949c5d82d3543767d4c8e171b6d04d</anchor>
      <arglist>(u, v)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>vec_cosine</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a50a987f1089170493bd8a392ba081249</anchor>
      <arglist>(u, v)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(rot_matrix_dim, rot_matrix_dim)</type>
      <name>rotmat_triplet</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a773d282ec9f1ca0ed001c983bfb7cc1b</anchor>
      <arglist>(phi1, PHI, phi2)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(rot_matrix_dim, rot_matrix_dim)</type>
      <name>rotmat_eulerangles</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8b0208f439a01392e10e97eb2b3f009d</anchor>
      <arglist>(ang)</arglist>
    </member>
    <member kind="function">
      <type>pure type(eulerangles) function</type>
      <name>euleranglestype</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad7da68e48442d8f3b70e43cc9a9672d3</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>rotatesrtensorto_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a90ecca88917124ec1b177d29f6b19abb</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>rotatesrtensorfrom_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ae285197b436d5889852b4254ae4f80a8</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure type(srtensor) function</type>
      <name>rotatesrtensorto_srtensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>adf21d35e7bbf9384e5799a2c17f5b3eb</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure type(srtensor) function</type>
      <name>rotatesrtensorfrom_srtensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa4d918e6eac574deb6ffbd8e1a7c765b</anchor>
      <arglist>(S, R)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_asymm_voigt_dim)</type>
      <name>mat33tovec3</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab707d3da563a51442612de2ce12aad10</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>vec3tomat33</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>af4918aeddcff3078779b93ddbc09bb9e</anchor>
      <arglist>(vec)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>vec6tomat33</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a9cd69660eace16f013f9818f9b843287</anchor>
      <arglist>(vec)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_symm_voigt_dim)</type>
      <name>mat33tovec6</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a5bbc8c129af6a094f67fb92104373d85</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_tensor_dim, sr_tensor_dim)</type>
      <name>vec9tomat33</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8b5bfaeb41852f58e3d9f9755b8bb251</anchor>
      <arglist>(vec)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(sr_voigt_dim)</type>
      <name>mat33tovec9</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>afae40a968b4e144498673698537573b7</anchor>
      <arglist>(mat)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>trace_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a2e3ffcb4fc4186951a786ffb6687bb02</anchor>
      <arglist>(X)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function</type>
      <name>trace_srtensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>af34cc731177198842472b2b4d5b07747</anchor>
      <arglist>(X)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>getnormalvector2d</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a08eb18077596eeb4e606682be7b3060c</anchor>
      <arglist>(A, B, length, v, beta)</arglist>
    </member>
    <member kind="function">
      <type>integer function</type>
      <name>solvequadraticpolynomial</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a0edef1d6669e63fbd6a6e812df4b7d93</anchor>
      <arglist>(a, b, c, x)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(3, 3)</type>
      <name>vec5d2tens</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a4e158616776e62e285ed385f4a85771e</anchor>
      <arglist>(v)</arglist>
    </member>
    <member kind="function">
      <type>pure double precision function, dimension(5)</type>
      <name>tens2vec5d</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad4899c6afd60edbd47366bf6aee64657</anchor>
      <arglist>(t)</arglist>
    </member>
    <member kind="variable">
      <type>type(srtensor), parameter</type>
      <name>unit_sr_tensor</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a85ddeba48fbdd4eb42641566db66f5bd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6a9b6fc89d6bee5567075f0b2064291f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad4918160e8df6c96122fbd76c0ba4869</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi_deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ade114c23d014db90bc9c439c7b128086</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>deg_pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab5753624d4216a85e366f556ba3c70cc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a398ac1586197cb4acfe8898840b0f86f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2i</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a2c2e1af846fb820b31cf71e4a9462c06</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root23</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8f5008cdecddc5422b823e7849e655d8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root32</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>adfb13757cc089db703babbe44cc9053f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_tensor_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a4c8bab38c29f02139ed7d3bfba642864</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>rot_matrix_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a90f99787840fe40d0bd3cd53f0fa1755</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_asymm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6b9e7a537da6ffeea6434748617dd48</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_symm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6f6ba7d874dd79d42873b2026b4d62c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a15d1e2e661ee56108c9518fd3864ff69</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(sr_tensor_dim, sr_tensor_dim), parameter</type>
      <name>unit_sr_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6149c01cf6f4e5850a34a8823324c8a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6a9b6fc89d6bee5567075f0b2064291f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ad4918160e8df6c96122fbd76c0ba4869</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>pi_deg</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ade114c23d014db90bc9c439c7b128086</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>deg_pi</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>ab5753624d4216a85e366f556ba3c70cc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a398ac1586197cb4acfe8898840b0f86f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root2i</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a2c2e1af846fb820b31cf71e4a9462c06</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root23</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a8f5008cdecddc5422b823e7849e655d8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, parameter</type>
      <name>root32</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>adfb13757cc089db703babbe44cc9053f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_tensor_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a4c8bab38c29f02139ed7d3bfba642864</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>rot_matrix_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a90f99787840fe40d0bd3cd53f0fa1755</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_asymm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6b9e7a537da6ffeea6434748617dd48</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_symm_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>aa6f6ba7d874dd79d42873b2026b4d62c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>sr_voigt_dim</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a15d1e2e661ee56108c9518fd3864ff69</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>double precision, dimension(sr_tensor_dim, sr_tensor_dim), parameter</type>
      <name>unit_sr_matrix</name>
      <anchorfile>namespacecrimathutils.html</anchorfile>
      <anchor>a6149c01cf6f4e5850a34a8823324c8a0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crinamedrange</name>
    <filename>namespacecrinamedrange.html</filename>
    <member kind="function">
      <type>class(range_type) function, pointer</type>
      <name>rangefactory</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a3eec7f47f4e78ed876939595d932c5cc</anchor>
      <arglist>(name)</arglist>
    </member>
    <member kind="function">
      <type>class(range_type) function, pointer</type>
      <name>rangefactory_extended</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a9c861a40683d02d87ca6847c83a5f7d6</anchor>
      <arglist>(name)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_uniform_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a34bb75e452f7cea5ff562504297f0121</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_biased_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a3d6c8f53a211ec2155d07eaa43a95fb1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_doublebiased_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>aa96429c1133a5157b4997255f32ca949</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_multibiased_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a8926f08c0b74156b94bc36220ba88f53</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_discrete_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a9424d32a3e4b4d1684c0e11a0db96c64</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(mapitem), dimension(range_ntypes), parameter</type>
      <name>range_name_map</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a504a00b38f22af4f39b2938012c88248</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>type(mapitem), dimension(range_nextensions), parameter</type>
      <name>range_name_extensions_map</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a87fd5db56a963e6794920aa841c6290a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_zero_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>aaa37f71d4dcfa06a392724bf7f0f2147</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_one_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a5e66c3741e45a8f55661c2279d7bceda</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_zero_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>aaa37f71d4dcfa06a392724bf7f0f2147</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>range_one_id</name>
      <anchorfile>namespacecrinamedrange.html</anchorfile>
      <anchor>a5e66c3741e45a8f55661c2279d7bceda</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crinumerics</name>
    <filename>namespacecrinumerics.html</filename>
    <class kind="type">crinumerics::barycentricinterpolator</class>
    <class kind="interface">crinumerics::interpolate</class>
    <class kind="interface">crinumerics::linspace</class>
    <member kind="function">
      <type>pure subroutine</type>
      <name>linspace_dynarr</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>ad7b8f7f9ef7f8735bfa84334af363dda</anchor>
      <arglist>(xstart, xend, n, array, endpoint)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>linspace_arr</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a560d856b796afe26e70ae52a731c53e3</anchor>
      <arglist>(xstart, xend, array, endpoint)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>barycentricinterpolator_init</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a66099be4de96c83c6a4eb0a90b5dd53e</anchor>
      <arglist>(this, order, xi, yi, info)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>barycentricinterpolator_init_allocate</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>aaae28aeffc6f4341ce08105fdf319493</anchor>
      <arglist>(this, order, npoints, info)</arglist>
    </member>
    <member kind="function">
      <type>double precision pure function</type>
      <name>barycentricinterpolator_interpolate</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a59cea91da1ef4c174d1a1a13e936f0e6</anchor>
      <arglist>(this, x)</arglist>
    </member>
    <member kind="function">
      <type>double precision pure function</type>
      <name>barycentric_interpolation</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>a6d5c29ffcf15b950a6b16b9fe73cf45c</anchor>
      <arglist>(x, xi, yi, wi)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>barycentric_weights</name>
      <anchorfile>namespacecrinumerics.html</anchorfile>
      <anchor>ab41904a084be454ee5d8cafdbd7ec832</anchor>
      <arglist>(xi, wi, info)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>cripath</name>
    <filename>namespacecripath.html</filename>
    <member kind="function">
      <type>pure character(len=len(path)) function</type>
      <name>basename</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>acf33272936fc9b3fb2a79f303d90b613</anchor>
      <arglist>(path)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(path)) function</type>
      <name>stripext</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>ab6284d364899aeac072beaf949182ea6</anchor>
      <arglist>(path)</arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>splitext</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a427e721cd0af1d4810238292fb521fc4</anchor>
      <arglist>(path, root, ext)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(prefix)+len(suffix)) function</type>
      <name>mkfilename</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a59882556ce377835f391465127e8120c</anchor>
      <arglist>(prefix, suffix)</arglist>
    </member>
    <member kind="function">
      <type>pure character(len=len(path_a)+len(path_b)) function</type>
      <name>pathjoin</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>aaf9b652b0fd1d069cbfab97daa69221e</anchor>
      <arglist>(path_a, path_b)</arglist>
    </member>
    <member kind="variable">
      <type>character, parameter</type>
      <name>pathsep</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a3b212c2ebc04a77130a7b47259cb431f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>max_pathlen</name>
      <anchorfile>namespacecripath.html</anchorfile>
      <anchor>a431565de6cbae010916555dd42fb8f5e</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>crirange</name>
    <filename>namespacecrirange.html</filename>
  </compound>
  <compound kind="namespace">
    <name>criruntime</name>
    <filename>namespacecriruntime.html</filename>
    <class kind="type">criruntime::commandline</class>
    <member kind="function">
      <type>subroutine</type>
      <name>finalize</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a63427cf84230e038e9283c85459955f2</anchor>
      <arglist>(errcode)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>processcommandline</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aee8edd047b0ba1d5a5f0067ffacfdf6c</anchor>
      <arglist>(this, argc_min, argc_max, command_map, command_argpos, info, terminate)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>finishprocessing</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>afa1548197c8e0c3d4b589cb7effa1e74</anchor>
      <arglist>(this, command_map, info, terminate)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>printhelpmessage</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>ae4940aef753d2b1322a27659dc0b8ade</anchor>
      <arglist>(this, command_map, info)</arglist>
    </member>
    <member kind="function">
      <type>subroutine</type>
      <name>getargv</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a7f6bc1a11847d6ec29643785f832ca12</anchor>
      <arglist>(argc, argv, info)</arglist>
    </member>
    <member kind="function">
      <type>integer function</type>
      <name>openordie</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>ae24680de8b2573d1d72ce20f42a9ef2d</anchor>
      <arglist>(fpath, status)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>errmsg_len</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aadf126359318d1657fa7dc4c80a2a053</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character(len=errmsg_len), save</type>
      <name>errmsg</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a6be05f9853d153a53e3f1511df7134ed</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>description_len</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>affe08f177754552f44d45bbb95a1c128</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>max_command_param_len</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>acf96fd012797ef564d36c611757c3adb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ok</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a4b23d17de3a3852fe7b1d26931cde693</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_inputerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aa35f23bf5c0993afba7de5a8f6b975d2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ioerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a35bbce4dcd8dfca411591b89ae418d9d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_runtimeerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a17e4716ebdaf69e8e87f7ca09b9325ab</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ok</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a4b23d17de3a3852fe7b1d26931cde693</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_inputerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>aa35f23bf5c0993afba7de5a8f6b975d2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_ioerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a35bbce4dcd8dfca411591b89ae418d9d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>stopcode_runtimeerror</name>
      <anchorfile>namespacecriruntime.html</anchorfile>
      <anchor>a17e4716ebdaf69e8e87f7ca09b9325ab</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>critest</name>
    <filename>namespacecritest.html</filename>
    <member kind="function">
      <type>subroutine, public</type>
      <name>testinit</name>
      <anchorfile>namespacecritest.html</anchorfile>
      <anchor>a5898f57d07c3ec16d6a299c676da8fbd</anchor>
      <arglist>(outunit)</arglist>
    </member>
    <member kind="function">
      <type>subroutine, public</type>
      <name>testsummary</name>
      <anchorfile>namespacecritest.html</anchorfile>
      <anchor>ac2a8e18b5e67c8331d64109719b0a997</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>subroutine, public</type>
      <name>testreport</name>
      <anchorfile>namespacecritest.html</anchorfile>
      <anchor>a72e58e58d57e4159f4e353fdd6f84688</anchor>
      <arglist>(name, outcome, line, file)</arglist>
    </member>
  </compound>
  <compound kind="namespace">
    <name>criuncomment</name>
    <filename>namespacecriuncomment.html</filename>
    <class kind="interface">criuncomment::readvalue</class>
    <member kind="function">
      <type>pure subroutine</type>
      <name>stripcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a6c550f9bbf2f78d61b11e833be9cd479</anchor>
      <arglist>(line, comment_mark)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>skipcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a5b192b07be757c83a4cb818aa133cd85</anchor>
      <arglist>(nunit, buffer)</arglist>
    </member>
    <member kind="variable">
      <type>integer, parameter</type>
      <name>max_line_len</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a54e44a42a767eb45f828b3328af1a82c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>character, parameter</type>
      <name>comment_sign</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a045aed2f669fb3efaa51564c32ad7abd</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>pure subroutine</type>
      <name>stripcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a6c550f9bbf2f78d61b11e833be9cd479</anchor>
      <arglist>(line, comment_mark)</arglist>
    </member>
    <member kind="function">
      <type>logical function</type>
      <name>skipcomment</name>
      <anchorfile>namespacecriuncomment.html</anchorfile>
      <anchor>a5b192b07be757c83a4cb818aa133cd85</anchor>
      <arglist>(nunit, buffer)</arglist>
    </member>
  </compound>
</tagfile>
