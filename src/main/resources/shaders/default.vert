#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
layout (location = 2) in vec3 aColor;       // stride >= 8 only; otherwise uses glVertexAttrib3f default (white)
layout (location = 3) in float aBoneIndex;  // stride == 6 only (skinned character meshes)
out vec2 TexCoord;
out vec3 VertColor;
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
// GPU skeletal animation: when skinned == true, each vertex is transformed from
// bone-local S3D space to GL space via its bone's pre-composed transform
// (boneWorldTransform with S3D→GL conversion baked in). The model matrix then
// applies world position/heading/scale on top.
uniform bool skinned = false;
uniform mat4 boneTransforms[64];
void main() {
    vec4 pos;
    if (skinned) {
        // aBoneIndex is a float in the VBO; round to nearest int for lookup
        int idx = int(aBoneIndex + 0.5);
        // boneTransforms[idx] = s3dToGlMatrix * boneWorldTransform
        // aPos is bone-local S3D coords, so result is GL-space relative to model root
        pos = boneTransforms[idx] * vec4(aPos, 1.0);
    } else {
        // Non-skinned: aPos is already in GL space (zone mesh, objects, static models)
        pos = vec4(aPos, 1.0);
    }
    gl_Position = projection * view * model * pos;
    TexCoord = aTexCoord;
    VertColor = aColor;
}
