<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.18.0-Zürich" styleCategories="AllStyleCategories" hasScaleBasedVisibilityFlag="0" minScale="1e+08" maxScale="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal mode="0" fetchMode="0" enabled="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <provider>
      <resampling zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" enabled="false" maxOversampling="2"/>
    </provider>
    <rasterrenderer nodataColor="" type="singlebandpseudocolor" classificationMax="0.125" classificationMin="-0.0531398378188493" alphaBand="11" opacity="1" band="1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>MinMax</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader maximumValue="0.125" classificationMode="1" colorRampType="INTERPOLATED" minimumValue="-0.053139837818849303" labelPrecision="4" clip="0">
          <colorramp type="gradient" name="[source]">
            <Option type="Map">
              <Option type="QString" name="color1" value="68,1,84,255"/>
              <Option type="QString" name="color2" value="253,231,37,255"/>
              <Option type="QString" name="discrete" value="0"/>
              <Option type="QString" name="rampType" value="gradient"/>
              <Option type="QString" name="stops" value="0.0196078;70,8,92,255:0.0392157;71,16,99,255:0.0588235;72,23,105,255:0.0784314;72,29,111,255:0.0980392;72,36,117,255:0.117647;71,42,122,255:0.137255;70,48,126,255:0.156863;69,55,129,255:0.176471;67,61,132,255:0.196078;65,66,135,255:0.215686;63,72,137,255:0.235294;61,78,138,255:0.254902;58,83,139,255:0.27451;56,89,140,255:0.294118;53,94,141,255:0.313725;51,99,141,255:0.333333;49,104,142,255:0.352941;46,109,142,255:0.372549;44,113,142,255:0.392157;42,118,142,255:0.411765;41,123,142,255:0.431373;39,128,142,255:0.45098;37,132,142,255:0.470588;35,137,142,255:0.490196;33,142,141,255:0.509804;32,146,140,255:0.529412;31,151,139,255:0.54902;30,156,137,255:0.568627;31,161,136,255:0.588235;33,165,133,255:0.607843;36,170,131,255:0.627451;40,174,128,255:0.647059;46,179,124,255:0.666667;53,183,121,255:0.686275;61,188,116,255:0.705882;70,192,111,255:0.72549;80,196,106,255:0.745098;90,200,100,255:0.764706;101,203,94,255:0.784314;112,207,87,255:0.803922;124,210,80,255:0.823529;137,213,72,255:0.843137;149,216,64,255:0.862745;162,218,55,255:0.882353;176,221,47,255:0.901961;189,223,38,255:0.921569;202,225,31,255:0.941176;216,226,25,255:0.960784;229,228,25,255:0.980392;241,229,29,255"/>
            </Option>
            <prop k="color1" v="68,1,84,255"/>
            <prop k="color2" v="253,231,37,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.0196078;70,8,92,255:0.0392157;71,16,99,255:0.0588235;72,23,105,255:0.0784314;72,29,111,255:0.0980392;72,36,117,255:0.117647;71,42,122,255:0.137255;70,48,126,255:0.156863;69,55,129,255:0.176471;67,61,132,255:0.196078;65,66,135,255:0.215686;63,72,137,255:0.235294;61,78,138,255:0.254902;58,83,139,255:0.27451;56,89,140,255:0.294118;53,94,141,255:0.313725;51,99,141,255:0.333333;49,104,142,255:0.352941;46,109,142,255:0.372549;44,113,142,255:0.392157;42,118,142,255:0.411765;41,123,142,255:0.431373;39,128,142,255:0.45098;37,132,142,255:0.470588;35,137,142,255:0.490196;33,142,141,255:0.509804;32,146,140,255:0.529412;31,151,139,255:0.54902;30,156,137,255:0.568627;31,161,136,255:0.588235;33,165,133,255:0.607843;36,170,131,255:0.627451;40,174,128,255:0.647059;46,179,124,255:0.666667;53,183,121,255:0.686275;61,188,116,255:0.705882;70,192,111,255:0.72549;80,196,106,255:0.745098;90,200,100,255:0.764706;101,203,94,255:0.784314;112,207,87,255:0.803922;124,210,80,255:0.823529;137,213,72,255:0.843137;149,216,64,255:0.862745;162,218,55,255:0.882353;176,221,47,255:0.901961;189,223,38,255:0.921569;202,225,31,255:0.941176;216,226,25,255:0.960784;229,228,25,255:0.980392;241,229,29,255"/>
          </colorramp>
          <item color="#440154" value="-0.125" label="-0.1250" alpha="255"/>
          <item color="#46085c" value="-0.12009805" label="-0.1201" alpha="255"/>
          <item color="#471063" value="-0.115196075" label="-0.1152" alpha="255"/>
          <item color="#481769" value="-0.110294125" label="-0.1103" alpha="255"/>
          <item color="#481d6f" value="-0.10539215" label="-0.1054" alpha="255"/>
          <item color="#482475" value="-0.1004902" label="-0.1005" alpha="255"/>
          <item color="#472a7a" value="-0.09558825" label="-0.0956" alpha="255"/>
          <item color="#46307e" value="-0.09068625" label="-0.0907" alpha="255"/>
          <item color="#453781" value="-0.08578425" label="-0.0858" alpha="255"/>
          <item color="#433d84" value="-0.08088225" label="-0.0809" alpha="255"/>
          <item color="#414287" value="-0.0759805" label="-0.0760" alpha="255"/>
          <item color="#3f4889" value="-0.0710785" label="-0.0711" alpha="255"/>
          <item color="#3d4e8a" value="-0.0661765" label="-0.0662" alpha="255"/>
          <item color="#3a538b" value="-0.0612745" label="-0.0613" alpha="255"/>
          <item color="#38598c" value="-0.0563725" label="-0.0564" alpha="255"/>
          <item color="#355e8d" value="-0.0514705" label="-0.0515" alpha="255"/>
          <item color="#33638d" value="-0.04656875" label="-0.0466" alpha="255"/>
          <item color="#31688e" value="-0.04166675" label="-0.0417" alpha="255"/>
          <item color="#2e6d8e" value="-0.03676475" label="-0.0368" alpha="255"/>
          <item color="#2c718e" value="-0.03186275" label="-0.0319" alpha="255"/>
          <item color="#2a768e" value="-0.02696075" label="-0.0270" alpha="255"/>
          <item color="#297b8e" value="-0.02205875" label="-0.0221" alpha="255"/>
          <item color="#27808e" value="-0.01715675" label="-0.0172" alpha="255"/>
          <item color="#25848e" value="-0.012255" label="-0.0123" alpha="255"/>
          <item color="#23898e" value="-0.007353" label="-0.0074" alpha="255"/>
          <item color="#218e8d" value="-0.00245099999999999" label="-0.0025" alpha="255"/>
          <item color="#20928c" value="0.00245100000000001" label="0.0025" alpha="255"/>
          <item color="#1f978b" value="0.007353" label="0.0074" alpha="255"/>
          <item color="#1e9c89" value="0.012255" label="0.0123" alpha="255"/>
          <item color="#1fa188" value="0.01715675" label="0.0172" alpha="255"/>
          <item color="#21a585" value="0.02205875" label="0.0221" alpha="255"/>
          <item color="#24aa83" value="0.02696075" label="0.0270" alpha="255"/>
          <item color="#28ae80" value="0.03186275" label="0.0319" alpha="255"/>
          <item color="#2eb37c" value="0.03676475" label="0.0368" alpha="255"/>
          <item color="#35b779" value="0.04166675" label="0.0417" alpha="255"/>
          <item color="#3dbc74" value="0.04656875" label="0.0466" alpha="255"/>
          <item color="#46c06f" value="0.0514705" label="0.0515" alpha="255"/>
          <item color="#50c46a" value="0.0563725" label="0.0564" alpha="255"/>
          <item color="#5ac864" value="0.0612745" label="0.0613" alpha="255"/>
          <item color="#65cb5e" value="0.0661765" label="0.0662" alpha="255"/>
          <item color="#70cf57" value="0.0710785" label="0.0711" alpha="255"/>
          <item color="#7cd250" value="0.0759805" label="0.0760" alpha="255"/>
          <item color="#89d548" value="0.08088225" label="0.0809" alpha="255"/>
          <item color="#95d840" value="0.08578425" label="0.0858" alpha="255"/>
          <item color="#a2da37" value="0.09068625" label="0.0907" alpha="255"/>
          <item color="#b0dd2f" value="0.09558825" label="0.0956" alpha="255"/>
          <item color="#bddf26" value="0.10049025" label="0.1005" alpha="255"/>
          <item color="#cae11f" value="0.10539225" label="0.1054" alpha="255"/>
          <item color="#d8e219" value="0.110294" label="0.1103" alpha="255"/>
          <item color="#e5e419" value="0.115196" label="0.1152" alpha="255"/>
          <item color="#f1e51d" value="0.120098" label="0.1201" alpha="255"/>
          <item color="#fde725" value="0.125" label="0.1250" alpha="255"/>
          <rampLegendSettings minimumLabel="" direction="0" maximumLabel="" orientation="2" suffix="" prefix="">
            <numericFormat id="basic">
              <Option type="Map">
                <Option type="QChar" name="decimal_separator" value=""/>
                <Option type="int" name="decimals" value="6"/>
                <Option type="int" name="rounding_type" value="0"/>
                <Option type="bool" name="show_plus" value="false"/>
                <Option type="bool" name="show_thousand_separator" value="true"/>
                <Option type="bool" name="show_trailing_zeros" value="false"/>
                <Option type="QChar" name="thousand_separator" value=""/>
              </Option>
            </numericFormat>
          </rampLegendSettings>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>
    <huesaturation colorizeStrength="100" colorizeRed="255" colorizeGreen="128" saturation="0" colorizeOn="0" grayscaleMode="0" colorizeBlue="128"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
