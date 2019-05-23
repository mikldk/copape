/**
 class_Individual.cpp
 Purpose: C++ class Individual.
 Details: C++ implementation.
  
 @author Mikkel Meyer Andersen
 */

/*
==========================================
Individual
==========================================
*/

#include "class_Individual.h"

Individual::Individual(int pid, int birthyear, bool surrogate) {
  m_pid = pid;
  m_birthyear = birthyear;
  m_surrogate = surrogate;
  
  m_father = nullptr;
}

int Individual::get_pid() const {
  return m_pid;
}

int Individual::get_birthyear() const {
  return m_birthyear;
}

bool Individual::is_surrogate() const {
  return m_surrogate;
}

std::shared_ptr<Individual> Individual::get_father() const {
  return m_father;
}

std::vector< std::shared_ptr<Individual> > Individual::get_children() const {
  return m_children;
}

void Individual::set_father(const Individual& father) {
  m_father = std::make_shared<Individual>(father);
}

void Individual::add_child(const Individual& child) {
  m_children.push_back( std::make_shared<Individual>(child) );
}

