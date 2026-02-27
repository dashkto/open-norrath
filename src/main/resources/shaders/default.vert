#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
layout (location = 2) in vec3 aColor;       // stride >= 8 only; otherwise uses glVertexAttrib3f default (white)
layout (location = 3) in float aBoneIndex;  // stride == 6 only (skinned character meshes)
layout (location = 4) in vec3 aNormal;      // separate normal VBO; default (0,1,0) via glVertexAttrib3f if none

out vec2 TexCoord;
out vec3 VertColor;
out vec3 FragNormal;
out vec4 FragPosLightSpace;

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
uniform mat4 lightSpaceMatrix;

// GPU skeletal animation: when skinned == true, each vertex is transformed from
// bone-local S3D space to GL space via its bone's pre-composed transform
// (boneWorldTransform with S3D→GL conversion baked in). The model matrix then
// applies world position/heading/scale on top.
uniform bool skinned = false;
uniform mat4 boneTransforms[64];

void main() {
    vec4 pos;
    vec3 normal;
    if (skinned) {
        // aBoneIndex is a float in the VBO; round to nearest int for lookup
        int idx = int(aBoneIndex + 0.5);
        // boneTransforms[idx] = s3dToGlMatrix * boneWorldTransform
        // aPos is bone-local S3D coords, so result is GL-space relative to model root
        pos = boneTransforms[idx] * vec4(aPos, 1.0);
        // Normal also in bone-local S3D space — same bone transform rotates it to GL space.
        // mat3() extracts upper-3x3 rotation (no non-uniform scale in EQ skeletons).
        normal = mat3(boneTransforms[idx]) * aNormal;
    } else {
        // Non-skinned: aPos and aNormal are already in GL space
        pos = vec4(aPos, 1.0);
        normal = aNormal;
    }
    vec4 worldPos = model * pos;
    gl_Position = projection * view * worldPos;
    TexCoord = aTexCoord;
    VertColor = aColor;
    // Normal to world space — model matrices use uniform scale only (rotation + uniform scale),
    // so mat3(model) is correct (no need for transpose-inverse).
    FragNormal = mat3(model) * normal;
    FragPosLightSpace = lightSpaceMatrix * worldPos;
}
