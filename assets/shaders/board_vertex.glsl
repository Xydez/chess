#version 330 core
layout (location = 0) in vec3 pos;

uniform vec3 white_color;
uniform vec3 black_color;
uniform mat4 mvp;

out vec4 out_color;

void main()
{
	int x = gl_InstanceID % 8;
	int y = gl_InstanceID / 8; // -
	gl_Position = mvp * vec4(pos.x + x / 4.0, -(pos.y + y / 4.0), pos.z, 1.0);

	if ((gl_InstanceID + y) % 2 == 0)
	{
		out_color = vec4(white_color, 1.0);
	}
	else
	{
		out_color = vec4(black_color, 1.0);
	}
}
