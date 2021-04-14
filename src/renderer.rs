use std::convert::TryInto;
use glfw::Context;
use image::GenericImageView;

fn error_callback(_: glfw::Error, description: String, error_count: &std::cell::Cell<usize>) {
    println!("GLFW error {}: {}", error_count.get(), description);
    error_count.set(error_count.get() + 1);
}

#[macro_export]
macro_rules! gl {
    ($($s:stmt;)*) => {
        $(
            $s
            if cfg!(debug_assertions) {
				#[allow(unused_unsafe)]
                let err = unsafe { gl::GetError() };
                if err != gl::NO_ERROR {
                    let err_str = match err {
                        gl::INVALID_ENUM => "GL_INVALID_ENUM",
                        gl::INVALID_VALUE => "GL_INVALID_VALUE",
                        gl::INVALID_OPERATION => "GL_INVALID_OPERATION",
                        gl::INVALID_FRAMEBUFFER_OPERATION => "GL_INVALID_FRAMEBUFFER_OPERATION",
                        gl::OUT_OF_MEMORY => "GL_OUT_OF_MEMORY",
                        gl::STACK_UNDERFLOW => "GL_STACK_UNDERFLOW",
                        gl::STACK_OVERFLOW => "GL_STACK_OVERFLOW",
                        _ => "unknown error"
                    };
                    println!("{}:{} - {} caused {}",
                             file!(),
                             line!(),
                             stringify!($s),
                             err_str);
                }
            }
        )*
    }
}

pub enum Event
{
	Resize(u32, u32),
	Key(glfw::Key, glfw::Action, glfw::Modifiers)
}

pub struct Window
{
	glfw: glfw::Glfw,
	window: glfw::Window,
	events: std::sync::mpsc::Receiver<(f64, glfw::WindowEvent)>,
	event_callback: Option<Box<dyn Fn(Event)>>
}

impl Window
{
	pub fn new(title: &str, width: u32, height: u32) -> Window
	{
		let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

		glfw.window_hint(glfw::WindowHint::Resizable(true));

		// Use OpenGL version 3.3
		glfw.window_hint(glfw::WindowHint::ContextVersion(3, 3));

		// Use the OpenGL core profile
		glfw.window_hint(glfw::WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

		#[cfg(debug_assertions)]
		glfw.window_hint(glfw::WindowHint::OpenGlDebugContext(true));

		let (mut window, events) = glfw.create_window(640, 480, "GlVoxel", glfw::WindowMode::Windowed).expect("Failed to create glfw window");

		window.set_key_polling(true);
		window.set_framebuffer_size_polling(true);
		window.make_current();

		glfw.set_swap_interval(glfw::SwapInterval::Sync(1)); // Disable vertical synchronization (vsync)

		glfw.set_error_callback(Some(glfw::Callback
		{
			f: error_callback,
			data: std::cell::Cell::new(0)
		}));

		gl!(gl::load_with(|s| window.get_proc_address(s)););

		return Window { glfw, window, events, event_callback: None };
	}

	pub fn set_event_handler(&mut self, callback: impl Fn(Event) + 'static)
	{
		self.event_callback = Some(Box::new(callback));
	}

	pub fn is_open(&self) -> bool
	{
		return !self.window.should_close();
	}

	pub fn close(&mut self)
	{
		self.window.set_should_close(true);
	}

	pub fn poll_events(&mut self)
	{
		self.glfw.poll_events();
		let events = glfw::flush_messages(&self.events);
		for (_, event) in events
		{
			match event
			{
				glfw::WindowEvent::Key(key, scancode, action, modifiers) =>
				{
					// TODO: The problem is that I want to be able to mutate the window in the callback, but since the window is being used to call the callback the window cannot be mutable again..? Basically, we're battling the rust compiler...
					match &mut self.event_callback
					{
						Some(callback) =>
						{
							callback(Event::Key(key, action, modifiers));
						},
						None => ()
					}
				},
				glfw::WindowEvent::FramebufferSize(width, height) =>
				{
					assert!(width >= 0 && height >= 0);

					match &mut self.event_callback
					{
						Some(callback) =>
						{
							callback(Event::Resize(width as u32, height as u32));
						},
						None => ()
					}
				},
				_ => ()
			}
		}
	}

	pub fn swap_buffers(&mut self)
	{
		self.window.swap_buffers();
	}

	pub fn get_time(&self) -> f64
	{
		return self.glfw.get_time();
	}
}

pub fn render_arrays(shader_program: &ShaderProgram, vertex_array: &VertexArray, vertex_count: usize) //, index_buffer: Option<IndexBuffer>
{
	// if index_buffer.is_some()
	unsafe
	{
		shader_program.bind();
		vertex_array.bind();

		gl!(gl::DrawArrays(gl::TRIANGLES, 0, vertex_count.try_into().unwrap()););
	}
}

pub fn render_elements(shader_program: &ShaderProgram, vertex_array: &VertexArray, index_buffer: &IndexBuffer)
{
	unsafe
	{
		shader_program.bind();
		vertex_array.bind();
		index_buffer.bind();

		gl!(gl::DrawElements(gl::TRIANGLES, index_buffer.count().try_into().unwrap(), gl::UNSIGNED_INT, std::ptr::null()););
	}
}

pub fn render_arrays_instanced(shader_program: &ShaderProgram, vertex_array: &VertexArray, vertex_count: usize, instances: usize) //, index_buffer: Option<IndexBuffer>
{
	// if index_buffer.is_some()
	unsafe
	{
		shader_program.bind();
		vertex_array.bind();

		gl!(gl::DrawArraysInstanced(gl::TRIANGLES, 0, vertex_count.try_into().unwrap(), instances as i32););
	}
}

pub fn render_elements_instanced(shader_program: &ShaderProgram, vertex_array: &VertexArray, index_buffer: &IndexBuffer, instances: usize)
{
	unsafe
	{
		shader_program.bind();
		vertex_array.bind();
		index_buffer.bind();

		gl!(gl::DrawElementsInstanced(gl::TRIANGLES, index_buffer.count().try_into().unwrap(), gl::UNSIGNED_INT, std::ptr::null(), instances as i32););
	}
}

#[derive(Debug)]
pub struct IndexBuffer
{
	handle: gl::types::GLuint,
	count: usize
}

impl Drop for IndexBuffer
{
	fn drop(&mut self)
	{
		unsafe
		{
			gl!(gl::DeleteBuffers(1, &self.handle););
		}
	}
}

impl IndexBuffer
{
	pub fn new(indices: &Vec<u32>) -> IndexBuffer
	{
		let mut handle = 0;

		unsafe
		{
			gl!(gl::GenBuffers(1, &mut handle););
			gl!(gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, handle););

			gl!(gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (indices.len() * std::mem::size_of::<u32>()) as isize, indices.as_ptr() as *const std::ffi::c_void, gl::STATIC_DRAW););

			gl!(gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, 0););
		}

		return IndexBuffer { handle, count: indices.len() };
	}

	pub unsafe fn bind(&self)
	{
		gl!(gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.handle););
	}

	pub unsafe fn unbind(&self)
	{
		gl!(gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, 0););
	}

	pub fn count(&self) -> usize
	{
		return self.count;
	}
}

#[derive(Debug, Copy, Clone)]
pub enum VertexBufferElement
{
	Float, U32, I32, U16, I16, U8, I8
}

#[derive(Debug)]
pub struct VertexArray
{
	handle: gl::types::GLuint
}

impl VertexArray
{
	pub fn new(vertex_buffer: &VertexBuffer, layout: &Vec<(VertexBufferElement, usize)>) -> VertexArray
	{
		assert!(layout.len() <= u32::MAX as usize);

		let mut handle = 0;

		unsafe
		{
			gl!(gl::GenVertexArrays(1, &mut handle););
			gl!(gl::BindVertexArray(handle););

			vertex_buffer.bind();
		}

		let mut stride = 0;
		
		for (element, count) in layout.iter()
		{
			let size = match element
			{
				VertexBufferElement::Float => 4,
				VertexBufferElement::U32 | VertexBufferElement::I32 => 4,
				VertexBufferElement::U16 | VertexBufferElement::I16 => 2,
				VertexBufferElement::U8  | VertexBufferElement::I8  => 1
			};

			stride += size * count;
		}

		let mut offset = 0;

		for (i, (element, count)) in layout.iter().enumerate()
		{
			assert!(*count <= 4);

			let gl_type = match element
			{
				VertexBufferElement::Float => gl::FLOAT,
				VertexBufferElement::U32 => gl::UNSIGNED_INT,
				VertexBufferElement::I32 => gl::INT,
				VertexBufferElement::U16 => gl::UNSIGNED_SHORT,
				VertexBufferElement::I16 => gl::SHORT,
				VertexBufferElement::U8 => gl::UNSIGNED_BYTE,
				VertexBufferElement::I8 => gl::BYTE
			};

			let size = match element
			{
				VertexBufferElement::Float => 4,
				VertexBufferElement::U32 | VertexBufferElement::I32 => 4,
				VertexBufferElement::U16 | VertexBufferElement::I16 => 2,
				VertexBufferElement::U8  | VertexBufferElement::I8  => 1
			};

			unsafe
			{
				gl!(gl::VertexAttribPointer(i as u32, (*count) as i32, gl_type, gl::FALSE, stride as i32, std::mem::transmute(offset)););
				gl!(gl::EnableVertexAttribArray(i as u32););
			}

			offset += size * count;
		}

		unsafe
		{
			gl!(gl::BindVertexArray(0););
			vertex_buffer.unbind();
		}

		return VertexArray { handle };
	}

	pub unsafe fn bind(&self)
	{
		gl!(gl::BindVertexArray(self.handle););
	}

	pub unsafe fn unbind(&self)
	{
		gl!(gl::BindVertexArray(0););
	}
}

#[derive(Debug)]
pub struct VertexBuffer
{
	handle: gl::types::GLuint,
	size: usize
}

impl Drop for VertexBuffer
{
	fn drop(&mut self)
	{
		unsafe
		{
			gl!(gl::DeleteBuffers(1, &self.handle););
		}
	}
}

impl VertexBuffer
{
	pub fn new(vertices: &Vec<f32>) -> VertexBuffer
	{
		let mut handle = 0;

		unsafe
		{
			gl!(gl::GenBuffers(1, &mut handle););
			gl!(gl::BindBuffer(gl::ARRAY_BUFFER, handle););

			gl!(gl::BufferData(gl::ARRAY_BUFFER, (vertices.len() * std::mem::size_of::<f32>()) as isize, vertices.as_ptr() as *const std::ffi::c_void, gl::STATIC_DRAW););

			gl!(gl::BindBuffer(gl::ARRAY_BUFFER, 0););
		}

		return VertexBuffer { handle, size: vertices.len() * std::mem::size_of::<f32>() };
	}

	pub unsafe fn bind(&self)
	{
		gl!(gl::BindBuffer(gl::ARRAY_BUFFER, self.handle););
	}

	pub unsafe fn unbind(&self)
	{
		gl!(gl::BindBuffer(gl::ARRAY_BUFFER, 0););
	}

	pub fn size(&self) -> usize
	{
		return self.size;
	}
}

use std::collections::HashMap;

#[derive(Debug)]
pub struct IOError;

#[derive(Debug)]
pub enum ShaderError
{
	Compile(String), Link(String), InvalidType, MissingShader(ShaderType), UniformNotFound
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ShaderType
{
	Vertex, Fragment, Geometry // Compute (requires OpenGL 4.3 or ARB_compute_shader)
}

#[derive(Debug, Clone)]
pub struct ShaderSource
{
	source: String
}

impl ShaderSource
{
	pub fn new(source: &str) -> ShaderSource
	{
		return ShaderSource { source: String::from(source) };
	}

	pub fn load(path: &str) -> std::io::Result<ShaderSource>
	{
		return match std::fs::read_to_string(path)
		{
			Ok(source) => Ok(ShaderSource { source }),
			Err(err) => Err(err)
		}
	}

	pub fn compile(&self, shader_type: ShaderType) -> Result<Shader, ShaderError>
	{
		let type_gl_enum = match shader_type
		{
			ShaderType::Vertex => gl::VERTEX_SHADER,
			ShaderType::Fragment => gl::FRAGMENT_SHADER,
			ShaderType::Geometry => gl::GEOMETRY_SHADER
		};

		let handle;

		unsafe
		{
			handle = gl::CreateShader(type_gl_enum);

			// &CString::new(self.source.as_bytes()).unwrap().as_ptr()
			gl!(gl::ShaderSource(handle, 1, std::mem::transmute(&self.source.as_ptr()), &(self.source.len() as i32)););
			gl!(gl::CompileShader(handle););
		}

		let mut success = 1;

		unsafe
		{
			gl!(gl::GetShaderiv(handle, gl::COMPILE_STATUS, &mut success););
		}

		if success == 1 // GL_TRUE
		{
			return Ok(Shader { handle, shader_type });
		}
		else
		{
			let mut len = 0;

			unsafe
			{
				gl!(gl::GetShaderiv(handle, gl::INFO_LOG_LENGTH, &mut len););
			}

			let mut buffer = Vec::<u8>::with_capacity(len as usize + 1);
			buffer.extend([b' '].iter().cycle().take(len as usize));

			let error_str = unsafe { std::ffi::CString::from_vec_unchecked(buffer) };

			unsafe
			{
				gl!(gl::GetShaderInfoLog(handle, len, std::ptr::null_mut(), error_str.as_ptr() as *mut gl::types::GLchar););
			}

			return Err(ShaderError::Compile(error_str.to_string_lossy().into_owned()));
		}
	}
}

pub struct Shader
{
	handle: gl::types::GLuint,
	shader_type: ShaderType
}

impl Drop for Shader
{
	fn drop(&mut self)
	{
		unsafe
		{
			gl!(gl::DeleteShader(self.handle););
		}
	}
}

pub struct ShaderProgramBuilder
{
	vertex: Option<Shader>,
	fragment: Option<Shader>,
	geometry: Option<Shader>
}

impl ShaderProgramBuilder
{
	pub fn new() -> Self
	{
		return ShaderProgramBuilder
		{
			vertex: None,
			fragment: None,
			geometry: None
		};
	}

	pub fn vertex(&mut self, shader: Shader) -> &mut Self
	{
		assert!(shader.shader_type == ShaderType::Vertex, "Type of vertex shader does not match");

		self.vertex = Some(shader);

		return self;
	}

	pub fn fragment(&mut self, shader: Shader) -> &mut Self
	{
		assert!(shader.shader_type == ShaderType::Fragment, "Type of fragment shader does not match");
	
		self.fragment = Some(shader);

		return self;
	}

	pub fn geometry(&mut self, shader: Shader) -> &mut Self
	{
		assert!(shader.shader_type == ShaderType::Geometry, "Type of geometry shader does not match");

		self.geometry = Some(shader);

		return self;
	}

	pub fn build(&self) -> Result<ShaderProgram, ShaderError>
	{
		if self.vertex.is_none()
		{
			return Err(ShaderError::MissingShader(ShaderType::Vertex));
		}

		if self.fragment.is_none()
		{
			return Err(ShaderError::MissingShader(ShaderType::Fragment));
		}

		let handle = unsafe { gl::CreateProgram() };
		
		unsafe
		{
			// Attach the shaders
			gl!(gl::AttachShader(handle, self.vertex.as_ref().unwrap().handle););
			gl!(gl::AttachShader(handle, self.fragment.as_ref().unwrap().handle););

			if self.geometry.is_some()
			{
				gl!(gl::AttachShader(handle, self.geometry.as_ref().unwrap().handle););
			}

			// Link the program
			gl!(gl::LinkProgram(handle););

			// Detach the shaders
			gl!(gl::DetachShader(handle, self.vertex.as_ref().unwrap().handle););
			gl!(gl::DetachShader(handle, self.fragment.as_ref().unwrap().handle););

			if self.geometry.is_some()
			{
				gl!(gl::DetachShader(handle, self.geometry.as_ref().unwrap().handle););
			}
		}

		let mut success = 1;

		unsafe
		{
			gl!(gl::GetProgramiv(handle, gl::LINK_STATUS, &mut success););
		}

		if success == 1
		{
			return Ok(ShaderProgram { handle, location_cache: HashMap::new() });
		}
		else
		{
			let mut len = 0;

			
			unsafe
			{
				gl!(gl::GetProgramiv(handle, gl::INFO_LOG_LENGTH, &mut len););
			}

			let mut buffer = Vec::<u8>::with_capacity(len as usize + 1);
			buffer.extend([b' '].iter().cycle().take(len as usize));

			let error_str = unsafe { std::ffi::CString::from_vec_unchecked(buffer) };

			unsafe
			{
				gl!(gl::GetProgramInfoLog(handle, len, std::ptr::null_mut(), error_str.as_ptr() as *mut gl::types::GLchar););
			}

			return Err(ShaderError::Link(error_str.to_string_lossy().into_owned()));
		}
	}
}

pub trait Uniform
{
	unsafe fn set_uniform(&self, id: gl::types::GLint);
}

impl Uniform for f32
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		gl!(gl::Uniform1f(id, *self as gl::types::GLfloat););
	}
}

impl Uniform for u32
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		gl!(gl::Uniform1ui(id, *self as gl::types::GLuint););
	}
}

impl Uniform for i32
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		gl!(gl::Uniform1i(id, *self as gl::types::GLint););
	}
}

#[derive(Debug)]
pub struct ShaderProgram
{
	handle: gl::types::GLuint,
	location_cache: HashMap<String, gl::types::GLint>
}

impl ShaderProgram
{
	pub unsafe fn bind(&self)
	{
		gl!(gl::UseProgram(self.handle););
	}

	pub unsafe fn unbind(&self)
	{
		gl!(gl::UseProgram(0););
	}

	pub fn set_uniform<T: Uniform>(&mut self, name: &str, value: &T) -> Result<(), ShaderError>
	{
		match self.location_cache.get(name)
		{
			Some(id) =>
			{
				unsafe
				{
					self.bind();
					value.set_uniform(*id);
					self.unbind();
				}

				return Ok(());
			},
			None =>
			{
				let c_name = std::ffi::CString::new(name).unwrap();
				let id = unsafe { gl::GetUniformLocation(self.handle, c_name.as_ptr() as *const gl::types::GLchar) };

				self.location_cache.insert(name.to_string(), id);

				if id == -1
				{
					return Err(ShaderError::UniformNotFound);
				}
				else
				{
					unsafe
					{
						self.bind();
						value.set_uniform(id);
						self.unbind();
					}

					return Ok(());
				}
			}
		}
	}
}

impl Drop for ShaderProgram
{
	fn drop(&mut self)
	{
		unsafe
		{
			gl!(gl::DeleteProgram(self.handle););
		}
	}
}

#[derive(Debug)]
pub struct Texture
{
	handle: gl::types::GLuint
}

const OPENGL_SLOT_LIMIT: u32 = 16;

impl Texture
{
	pub fn load(path: &std::path::Path) -> Result<Texture, Box<dyn std::error::Error>>
	{
		let mut handle = 0;
		unsafe
		{
			gl!(gl::GenTextures(1, &mut handle););

			gl!(gl::ActiveTexture(gl::TEXTURE0););
			gl!(gl::BindTexture(gl::TEXTURE_2D, handle););

			gl!(gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::REPEAT as i32););
			gl!(gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::REPEAT as i32););

			gl!(gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32););
			gl!(gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32););
		}

		let image = image::open(&path)?.into_rgba8();
		let width = image.width();
		let height = image.height();

		unsafe
		{
			let image_data = image.into_raw();
			let data_ptr = image_data.as_ptr() as *const std::ffi::c_void;
			gl!(gl::TexImage2D(gl::TEXTURE_2D, 0, gl::RGBA as i32, width as i32, height as i32, 0, gl::RGBA, gl::UNSIGNED_BYTE, data_ptr););
			//gl!(gl::GenerateMipmap(gl::TEXTURE_2D););
		}

		return Ok(Texture { handle });
	}

	pub unsafe fn bind(texture: &Texture, slot: u32)
	{
		assert!(slot < OPENGL_SLOT_LIMIT);
		gl!(gl::ActiveTexture(gl::TEXTURE0 + slot););
		gl!(gl::BindTexture(gl::TEXTURE_2D, texture.handle););
	}

	pub unsafe fn unbind(slot: u32)
	{
		assert!(slot < OPENGL_SLOT_LIMIT);
		gl!(gl::ActiveTexture(gl::TEXTURE0 + slot););
		gl!(gl::BindTexture(gl::TEXTURE_2D, 0););
	}
}

impl Drop for Texture
{
	fn drop(&mut self)
	{
		unsafe
		{
			gl!(gl::DeleteTextures(1, &mut self.handle););
		}
	}
}

use std::sync::Mutex;
use lazy_static::lazy_static;

lazy_static!
{
	static ref SLOT_CACHE: Mutex<Vec<(gl::types::GLuint, u32, gl::types::GLint)>> = Mutex::new(Vec::new());
}

impl Uniform for Texture
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		let mut slot_cache = SLOT_CACHE.lock().unwrap();

		// First we check if this texture is bound to a slot, if so we use that slot
		for (handle, slot, _) in slot_cache.iter()
		{
			if *handle == self.handle
			{
				Texture::bind(&self, *slot);
				(*slot as i32).set_uniform(id);
				return;
			}
		}
		
		// This texture is not bound to a slot, so we need to bind the texture to a slot. First we find a suitable slot.
		for i in 0..OPENGL_SLOT_LIMIT
		{
			// Check if the slot is in the slot cache
			let found_slot = slot_cache.iter().find(|(handle, slot, bound_id)| *slot == i);
			if found_slot.is_none()
			{
				Texture::bind(&self, i);

				println!("Texture uniform (handle = {}, slot = {}, id = {})", self.handle, i, id);
				(i as i32).set_uniform(id);

				slot_cache.push((self.handle, i, id));

				break;
			}
		}
	}
}

#[derive(Debug)]
pub struct Mesh
{
	pub vao: VertexArray,
	pub vbo: VertexBuffer,
	pub ibo: IndexBuffer
}

impl Mesh
{
	pub fn new(vertices: &Vec<f32>, indices: &Vec<u32>) -> Mesh
	{
		assert!(vertices.len() % 3 == 0, "Vertices are not properly aligned with layout");

		return Mesh::with_layout(&vertices, &indices, &vec![(VertexBufferElement::Float, 3)]);
	}

	pub fn with_layout(vertices: &Vec<f32>, indices: &Vec<u32>, layout: &Vec<(VertexBufferElement, usize)>) -> Mesh
	{
		let mut layout_len = 0;
		for (_, size) in layout.iter()
		{
			layout_len += size;
		}

		assert!(vertices.len() % layout_len == 0, "Vertices are not properly aligned with layout");

		let vbo = VertexBuffer::new(vertices);
		let ibo = IndexBuffer::new(indices);

		let vao = VertexArray::new(&vbo, &layout);

		return Mesh { vao, vbo, ibo };
	}
}
