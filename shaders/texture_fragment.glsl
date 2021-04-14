#version 330

in vec2 tex_coord;

out vec4 fragColor;

uniform sampler2D u_texture_in;

void main()
{
	//fragColor = vec4(0.0, 0.0, 0.0, 1.0);
	fragColor = texture(u_texture_in, tex_coord);
}
