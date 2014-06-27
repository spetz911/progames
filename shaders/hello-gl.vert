#version 110

attribute vec3 position;
attribute vec2 texcoordX;
varying vec2 texcoord;

void main()
{
    gl_Position = vec4(position, 1.0);
    texcoord = texcoordX;
}
