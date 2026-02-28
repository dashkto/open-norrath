#version 330 core
in vec2 TexCoord;
in vec3 VertColor;
in vec3 FragNormal;
in vec4 FragPosLightSpace;

out vec4 FragColor;

uniform sampler2D tex0;
uniform sampler2DShadow shadowMap;  // hardware PCF via GL_COMPARE_REF_TO_TEXTURE

uniform bool enableLighting = false; // false for screens that don't set up lighting
uniform bool enableShadows = false; // false for screens without a shadow map bound (e.g. character preview)
uniform vec3 lightDir;              // points FROM sun TOWARD scene (normalized by caller or here)
uniform vec3 lightColor = vec3(1.0, 1.0, 1.0); // sun/moon tint (warm at dawn/dusk, cool at night)
uniform float ambientStrength = 0.35;
uniform float alphaMultiplier = 1.0;
uniform bool alphaTest = false;

// 3x3 PCF shadow sampling. sampler2DShadow returns 1.0 when NOT in shadow (ref <= stored depth).
float computeShadow(vec4 fragPosLS) {
    // Perspective divide → NDC, then remap [-1,1] → [0,1] for texture lookup
    vec3 proj = fragPosLS.xyz / fragPosLS.w;
    proj = proj * 0.5 + 0.5;

    // Fragments outside the shadow frustum → no shadow
    if (proj.z > 1.0) return 0.0;

    // Slope-scale bias: steeper surfaces need more bias to prevent shadow acne
    vec3 sunDir = normalize(-lightDir);
    float cosTheta = max(dot(normalize(FragNormal), sunDir), 0.0);
    float bias = max(0.005 * (1.0 - cosTheta), 0.001);

    // 3x3 PCF kernel — each texture() call returns 0.0 (shadowed) or 1.0 (lit)
    // because the depth texture has GL_COMPARE_REF_TO_TEXTURE with GL_LEQUAL
    float shadow = 0.0;
    vec2 texelSize = 1.0 / textureSize(shadowMap, 0);
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            shadow += texture(shadowMap, vec3(proj.xy + vec2(x, y) * texelSize, proj.z - bias));
        }
    }
    shadow /= 9.0;  // average: 1.0 = fully lit, 0.0 = fully shadowed

    return 1.0 - shadow;  // invert: 0.0 = lit, 1.0 = shadowed
}

void main() {
    vec4 texColor = texture(tex0, TexCoord) * vec4(VertColor, 1.0);
    texColor.a *= alphaMultiplier;
    if (alphaTest && texColor.a < 0.1) discard;

    if (!enableLighting) {
        // No lighting setup (splash screen, character preview) — pass through
        FragColor = texColor;
        return;
    }

    // Diffuse lighting from directional sun
    vec3 norm = normalize(FragNormal);
    vec3 sunDir = normalize(-lightDir);  // toward sun
    float diff = max(dot(norm, sunDir), 0.0);

    // Shadow (only when a shadow map is bound)
    float shadow = enableShadows ? computeShadow(FragPosLightSpace) : 0.0;

    // Combine: ambient always on, diffuse attenuated by shadow, tinted by light color
    float lighting = ambientStrength + (1.0 - ambientStrength) * diff * (1.0 - shadow);
    vec3 tintedLight = mix(vec3(1.0), lightColor, 1.0 - ambientStrength);

    FragColor = vec4(texColor.rgb * lighting * tintedLight, texColor.a);
}
