-- Calculate FPS
FPScounter = 1 + (FPScounter or 0) -- increment FPScounter (which is nil, when undefined, therefore "or 0") 
if os.time() > (LastTime or 0) then
  LastTime = os.time()
  print("FPS:    " .. FPScounter - (LastFPScounter or 0)) -- print is only displayed, if compiled with DEBUG
  LastFPScounter = FPScounter
  -- Show the first 200 Textures
  TexNum = 1 + (TexNum or 0)
  if TexNum > 200 then TexNum = 1 end
  print("TexNum: " .. (TexNum or 0))
end

-- Draw some rectangles
gl.Enable("GL_BLEND")
gl.Color(1, 1, 1, 0.5)
for i = 1,10 do
  gl.Begin("GL_LINE_loop")
  gl.Vertex(0+4*i, 0+4*i);
  gl.Vertex(0+4*i, 600-4*i);
  gl.Vertex(800-4*i, 600-4*i);
  gl.Vertex(800-4*i, 0+4*i);
  gl.End()
end
gl.Disable("GL_BLEND")

-- Display a Texture
gl.Enable("GL_BLEND")
gl.Enable("GL_TEXTURE_2D")
gl.Color(1, 1, 1, 1)
gl.BindTexture("GL_TEXTURE_2D", TexNum or 0)
gl.Begin("GL_QUADS")
gl.TexCoord(0, 0); gl.Vertex(10,  10);
gl.TexCoord(0, 1); gl.Vertex(10,  110);
gl.TexCoord(1, 1); gl.Vertex(110, 110);
gl.TexCoord(1, 0); gl.Vertex(110, 10);
gl.End()
gl.Disable("GL_TEXTURE_2D")
gl.Disable("GL_BLEND")
