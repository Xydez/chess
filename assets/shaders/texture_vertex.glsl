#version 330 core
layout (location = 0) in vec3 a_pos;
layout (location = 1) in vec2 a_tex_coord;

uniform mat4 u_mvp;

out vec2 tex_coord;

void main()
{
	gl_Position = u_mvp * vec4(a_pos.xy, 0.0, 1.0);

	tex_coord = a_tex_coord;
}
