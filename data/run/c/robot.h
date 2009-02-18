enum Case {
  laby_name_Void,
  laby_name_Wall,
  laby_name_Rock,
  laby_name_Web,
  laby_name_Exit
};

extern void laby_name_left();
extern void laby_name_right();
extern void laby_name_forward();
extern enum Case laby_name_look();
extern void laby_name_door_open();
extern void laby_name_take();
extern void laby_name_drop();
