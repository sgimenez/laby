class Robot
  laby_name_Void = 0
  laby_name_Wall = 1
  laby_name_Rock = 2
  laby_name_Web = 3
  laby_name_Exit = 4
  laby_name_Unknown = 5

  def initialize
    perform "start"
    laby_name_ant
  end

  def output s
    puts s
    $stdout.flush
  end

  def input
    gets rescue ""
  end

  def perform action
    output action
    input
  end

  def laby_name_look
    tile = perform('look')
    self.class.const_get tile rescue laby_name_Unknown
  end

  def laby_name_left; perform 'left'; end
  def laby_name_right; perform 'right'; end
  def laby_name_forward; perform 'forward'; end
  def laby_name_take; perform 'take'; end
  def laby_name_drop; perform 'drop'; end
  def laby_name_escape; perform 'escape'; end

  def laby_name_say s; perform ('say ' + s) end
end
