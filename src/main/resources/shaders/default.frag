#version 330 core
in vec2 TexCoord;
in vec3 VertColor;
out vec4 FragColor;
uniform sampler2D tex0;
uniform float alphaMultiplier = 1.0;
void main() {
    FragColor = texture(tex0, TexCoord) * vec4(VertColor, 1.0);
    FragColor.a *= alphaMultiplier;
    if (FragColor.a < 0.1) discard;
}
