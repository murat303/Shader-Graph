//#ifndef TESSELLATION_CGINC_INCLUDED
//#define TESSELLATION_CGINC_INCLUDED
#if defined(SHADER_API_D3D11) || defined(SHADER_API_GLES3) || defined(SHADER_API_GLCORE) || defined(SHADER_API_VULKAN) || defined(SHADER_API_METAL) || defined(SHADER_API_PSSL)
    #define UNITY_CAN_COMPILE_TESSELLATION 1
    #   define UNITY_domain                 domain
    #   define UNITY_partitioning           partitioning
    #   define UNITY_outputtopology         outputtopology
    #   define UNITY_patchconstantfunc      patchconstantfunc
    #   define UNITY_outputcontrolpoints    outputcontrolpoints
#endif

struct Varyings
{       
    float3 worldPos : TEXCOORD1;
    float3 normal : NORMAL;
    float4 vertex : SV_POSITION;
    float2 uv : TEXCOORD0;
    float3 viewDir : TEXCOORD3;
    float fogFactor : TEXCOORD4;
};

float _Tess;
float _MaxTessDistance;

struct TessellationFactors
{
    float edge[3] : SV_TessFactor;
    float inside : SV_InsideTessFactor;
};

struct Attributes
{
    float4 vertex : POSITION;
    float3 normal : NORMAL;
    float2 uv : TEXCOORD0;    
};

struct ControlPoint
{
    float4 vertex : INTERNALTESSPOS;
    float2 uv : TEXCOORD0;
    float3 normal : NORMAL;   
};

[UNITY_domain("tri")]
[UNITY_outputcontrolpoints(3)]
[UNITY_outputtopology("triangle_cw")]
[UNITY_partitioning("fractional_odd")]
[UNITY_patchconstantfunc("patchConstantFunction")]
ControlPoint hull(InputPatch<ControlPoint, 3> patch, uint id : SV_OutputControlPointID)
{
    return patch[id];
}

TessellationFactors UnityCalcTriEdgeTessFactors (float3 triVertexFactors)
{
    TessellationFactors tess;
    tess.edge[0] = 0.5 * (triVertexFactors.y + triVertexFactors.z);
    tess.edge[1] = 0.5 * (triVertexFactors.x + triVertexFactors.z);
    tess.edge[2] = 0.5 * (triVertexFactors.x + triVertexFactors.y);
    tess.inside = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
    return tess;
}

float CalcDistanceTessFactor(float4 vertex, float minDist, float maxDist, float tess)
{
				float3 worldPosition = mul(unity_ObjectToWorld, vertex).xyz;
				float dist = distance(worldPosition, _WorldSpaceCameraPos);
				float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0);
				return f * tess;
}

TessellationFactors DistanceBasedTess(float4 v0, float4 v1, float4 v2, float minDist, float maxDist, float tess)
{
				float3 f;
				f.x = CalcDistanceTessFactor(v0, minDist, maxDist, tess);
				f.y = CalcDistanceTessFactor(v1, minDist, maxDist, tess);
				f.z = CalcDistanceTessFactor(v2, minDist, maxDist, tess);

				return UnityCalcTriEdgeTessFactors(f);
}

uniform float3 _Position;
uniform sampler2D _GlobalEffectRT;
uniform float _OrthographicCamSize;

sampler2D  _Noise;
float _NoiseScale, _SnowHeight, _NoiseWeight, _SnowDepth;

TessellationFactors patchConstantFunction(InputPatch<ControlPoint, 3> patch)
{
    float minDist = 2.0;
    float maxDist = _MaxTessDistance;
    TessellationFactors f;
    return DistanceBasedTess(patch[0].vertex, patch[1].vertex, patch[2].vertex, minDist, maxDist, _Tess);
   
}

float4 GetShadowPositionHClip(Attributes input)
{
    float3 positionWS = TransformObjectToWorld(input.vertex.xyz);
    float3 normalWS = TransformObjectToWorldNormal(input.normal);
 
    float4 positionCS = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, 0));
 
#if UNITY_REVERSED_Z
    positionCS.z = min(positionCS.z, positionCS.w * UNITY_NEAR_CLIP_VALUE);
#else
    positionCS.z = max(positionCS.z, positionCS.w * UNITY_NEAR_CLIP_VALUE);
#endif
    return positionCS;
}

Varyings vert(Attributes input)
{
    Varyings output;
    
    float3 worldPosition = mul(unity_ObjectToWorld, input.vertex).xyz;
    //create local uv
    float2 uv = worldPosition.xz - _Position.xz;
    uv = uv / (_OrthographicCamSize * 2);
    uv += 0.5;
    
    // Effects RenderTexture Reading
    float4 RTEffect = tex2Dlod(_GlobalEffectRT, float4(uv, 0, 0));
    // smoothstep to prevent bleeding
   	RTEffect *=  smoothstep(0.99, 0.9, uv.x) * smoothstep(0.99, 0.9,1- uv.x);
	RTEffect *=  smoothstep(0.99, 0.9, uv.y) * smoothstep(0.99, 0.9,1- uv.y);
    
    // worldspace noise texture
    float SnowNoise = tex2Dlod(_Noise, float4(worldPosition.xz * _NoiseScale, 0, 0)).r;
    output.viewDir = SafeNormalize(GetCameraPositionWS() - worldPosition);

	// move vertices up where snow is
	input.vertex.xyz += SafeNormalize(input.normal) * saturate(( _SnowHeight) + (SnowNoise * _NoiseWeight)) * saturate(1-(RTEffect.g * _SnowDepth));

    // transform to clip space
    #ifdef SHADERPASS_SHADOWCASTER
        output.vertex = GetShadowPositionHClip(input);
    #else
        output.vertex = TransformObjectToHClip(input.vertex.xyz);
    #endif

    //outputs
    output.worldPos =  mul(unity_ObjectToWorld, input.vertex).xyz;
    output.normal = input.normal;
    output.uv = input.uv;
    output.fogFactor = ComputeFogFactor(output.vertex.z);
    return output;
}

[UNITY_domain("tri")]
Varyings domain(TessellationFactors factors, OutputPatch<ControlPoint, 3> patch, float3 barycentricCoordinates : SV_DomainLocation)
{
    Attributes v;
    
    #define Interpolate(fieldName) v.fieldName = \
				patch[0].fieldName * barycentricCoordinates.x + \
				patch[1].fieldName * barycentricCoordinates.y + \
				patch[2].fieldName * barycentricCoordinates.z;

    Interpolate(vertex)
    Interpolate(uv)
    Interpolate(normal)
    
    return vert(v);
}