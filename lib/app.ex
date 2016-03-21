defmodule Skyline.AppConfig do
    @doc false
  
    defstruct auth_opts: nil,
              router: nil
            
    @doc false
    defmacro __using__(_) do
     quote do
       @before_compile unquote(__MODULE__)
     end
    end

end
