#version 330 core
in vec2 vUV;
uniform sampler2D fontAtlas;
uniform vec4 color;
uniform int isText;
out vec4 fragColor;
void main() {
    if (isText == 1) {
        float alpha = texture(fontAtlas, vUV).r;
        fragColor = vec4(color.rgb, color.a * alpha);
    } else {
        fragColor = color;
    }
}
