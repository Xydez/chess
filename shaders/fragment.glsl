#version 330

out vec4 fragColor;

in vec4 out_color;

void main()
{
	fragColor = out_color;
}
