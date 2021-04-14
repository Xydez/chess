#version 330 core
layout (location = 0) in vec3 pos;

uniform vec3 color;
uniform mat4 mvp;

out vec4 out_color;

void main()
{
	gl_Position = mvp * vec4(pos.x, pos.y, 0.0, 1.0);

	out_color = vec4(color, 0.5);
}
