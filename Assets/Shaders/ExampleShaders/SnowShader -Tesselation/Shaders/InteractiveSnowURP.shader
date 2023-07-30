Shader "Custom/Snow Interactive" {
    Properties{
        [Header(Main)]
        _Noise("Snow Noise", 2D) = "gray" {}
        _NoiseScale("Noise Scale", Range(0,2)) = 0.1
        _NoiseWeight("Noise Weight", Range(0,2)) = 0.1
        [HDR]_ShadowColor("Shadow Color", Color) = (0.5,0.5,0.5,1)

        [Space]
        [Header(Tesselation)]
        _MaxTessDistance("Max Tessellation Distance", Range(10,100)) = 50
        _Tess("Tessellation", Range(1,32)) = 20

        [Space]
        [Header(Snow)]
        [HDR]_Color("Snow Color", Color) = (0.5,0.5,0.5,1)
        [HDR]_PathColorIn("Snow Path Color In", Color) = (0.5,0.5,0.7,1)
        [HDR]_PathColorOut("Snow Path Color Out", Color) = (0.5,0.5,0.7,1)
        _PathBlending("Snow Path Blending", Range(0,3)) = 0.3
        _MainTex("Snow Texture", 2D) = "white" {}
        _SnowHeight("Snow Height", Range(0,2)) = 0.3
        _SnowDepth("Snow Path Depth", Range(0,100)) = 0.3
        _SnowTextureOpacity("Snow Texture Opacity", Range(0,2)) = 0.3
        _SnowTextureScale("Snow Texture Scale", Range(0,2)) = 0.3

        [Space]
        [Header(Sparkles)]
        _SparkleScale("Sparkle Scale", Range(0,10)) = 10
        _SparkCutoff("Sparkle Cutoff", Range(0,10)) = 0.8
        _SparkleNoise("Sparkle Noise", 2D) = "gray" {}

        [Space]
        [Header(Rim)]
        _RimPower("Rim Power", Range(0,20)) = 20
        [HDR]_RimColor("Rim Color Snow", Color) = (0.5,0.5,0.5,1)
    }
    HLSLINCLUDE

    // Includes
    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"
    #include "SnowTessellation.hlsl"
    #pragma require tessellation tessHW
    #pragma vertex TessellationVertexProgram
    #pragma hull hull
    #pragma domain domain
    // Keywords
    
    #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
    #pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
    #pragma multi_compile _ _SHADOWS_SOFT
    #pragma multi_compile_fog


    ControlPoint TessellationVertexProgram(Attributes v)
    {
        ControlPoint p;
        p.vertex = v.vertex;
        p.uv = v.uv;
        p.normal = v.normal;
        return p;
    }
    ENDHLSL

    SubShader{
        Tags{ "RenderType" = "Opaque" "RenderPipeline" = "UniversalPipeline"}

        Pass{
            Tags { "LightMode" = "UniversalForward" }

            HLSLPROGRAM
            // vertex happens in snowtessellation.hlsl
            #pragma fragment frag
            #pragma target 4.0
            

            sampler2D _MainTex, _SparkleNoise;
            float4 _Color, _RimColor;
            float _RimPower;
            float4 _PathColorIn, _PathColorOut;
            float _PathBlending;
            float _SparkleScale, _SparkCutoff;
            float _SnowTextureOpacity, _SnowTextureScale;
            float4 _ShadowColor;
            

            half4 frag(Varyings IN) : SV_Target{

                // Effects RenderTexture Reading
                float3 worldPosition = mul(unity_ObjectToWorld, IN.vertex).xyz;
                float2 uv = IN.worldPos.xz - _Position.xz;
                uv /= (_OrthographicCamSize * 2);
                uv += 0.5;

                // effects texture				
                float4 effect = tex2D(_GlobalEffectRT, uv);

                // mask to prevent bleeding
                effect *=  smoothstep(0.99, 0.9, uv.x) * smoothstep(0.99, 0.9,1- uv.x);
                effect *=  smoothstep(0.99, 0.9, uv.y) * smoothstep(0.99, 0.9,1- uv.y);

                // worldspace Noise texture
                float3 topdownNoise = tex2D(_Noise, IN.worldPos.xz * _NoiseScale).rgb;

                // worldspace Snow texture
                float3 snowtexture = tex2D(_MainTex, IN.worldPos.xz * _SnowTextureScale).rgb;
                
                //lerp between snow color and snow texture
                float3 snowTex = lerp(_Color.rgb,snowtexture * _Color.rgb, _SnowTextureOpacity);
                
                //lerp the colors using the RT effect path 
                float3 path = lerp(_PathColorOut.rgb * effect.g, _PathColorIn.rgb, saturate(effect.g * _PathBlending));
                float3 mainColors = lerp(snowTex,path, saturate(effect.g));

                // lighting and shadow information
                float shadow = 0;
                half4 shadowCoord = TransformWorldToShadowCoord(IN.worldPos);
                
                #if _MAIN_LIGHT_SHADOWS_CASCADE || _MAIN_LIGHT_SHADOWS
                    Light mainLight = GetMainLight(shadowCoord);
                    shadow = mainLight.shadowAttenuation;
                #else
                    Light mainLight = GetMainLight();
                #endif

                // extra point lights support
                float3 extraLights;
                int pixelLightCount = GetAdditionalLightsCount();
                for (int j = 0; j < pixelLightCount; ++j) {
                    Light light = GetAdditionalLight(j, IN.worldPos, half4(1, 1, 1, 1));
                    float3 attenuatedLightColor = light.color * (light.distanceAttenuation * light.shadowAttenuation);
                    extraLights += attenuatedLightColor;			
                }

                float4 litMainColors = float4(mainColors,1) ;
                extraLights *= litMainColors.rgb;
                // add in the sparkles
                float sparklesStatic = tex2D(_SparkleNoise, IN.worldPos.xz * _SparkleScale).r;
                float cutoffSparkles = step(_SparkCutoff,sparklesStatic);				
                litMainColors += cutoffSparkles  *saturate(1- (effect.g * 2)) * 4;
                
                
                // add rim light
                half rim = 1.0 - dot((IN.viewDir), IN.normal) * topdownNoise.r;
                litMainColors += _RimColor * pow(abs(rim), _RimPower);

                // ambient and mainlight colors added
                half4 extraColors;
                extraColors.rgb = litMainColors.rgb * mainLight.color.rgb * (shadow + unity_AmbientSky.rgb);
                extraColors.a = 1;
                
                // colored shadows
                float3 coloredShadows = (shadow + (_ShadowColor.rgb * (1-shadow)));
                litMainColors.rgb = litMainColors.rgb * mainLight.color * (coloredShadows);
                // everything together
                float4 final = litMainColors+ extraColors + float4(extraLights,0);
                // add in fog
                final.rgb = MixFog(final.rgb, IN.fogFactor);
                return final;

            }
            ENDHLSL

        }

        // casting shadows is a little glitchy, I've turned it off, but maybe in future urp versions it works better?
        // Shadow Casting Pass
        // Pass
        // {
            // 	Name "ShadowCaster"
            // 	Tags { "LightMode" = "ShadowCaster" }
            // 	ZWrite On
            // 	ZTest LEqual
            // 	Cull Off
            
            // 	HLSLPROGRAM
            // 	#pragma target 3.0
            
            // 	// Support all the various light  ypes and shadow paths
            // 	#pragma multi_compile_shadowcaster
            
            // 	// Register our functions
            
            // 	#pragma fragment frag
            // 	// A custom keyword to modify logic during the shadow caster pass

            // 	half4 frag(Varyings IN) : SV_Target{
                // 		return 0;
            // 	}
            
            // 	ENDHLSL
        // }
    }
}