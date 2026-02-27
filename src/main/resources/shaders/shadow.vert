#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 3) in float aBoneIndex;  // only used when skinned == true

uniform mat4 lightSpaceMatrix;
uniform mat4 model;
uniform bool skinned = false;
uniform mat4 boneTransforms[64];

void main() {
    vec4 pos;
    if (skinned) {
        int idx = int(aBoneIndex + 0.5);
        pos = boneTransforms[idx] * vec4(aPos, 1.0);
    } else {
        pos = vec4(aPos, 1.0);
    }
    gl_Position = lightSpaceMatrix * model * pos;
}
