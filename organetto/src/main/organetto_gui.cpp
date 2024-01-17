#include <vector>
#include <string>

#include <SFML/Graphics.hpp>
#include <SFGUI/SFGUI.hpp>
#include <SFGUI/Widgets.hpp>

extern "C" {
#include "impl/common.h"
#include "impl/organetto.h"
#include "impl/controls.h"
}

std::vector<sfg::Adjustment::Ptr> drawbar_adjs;
std::string drawbar_labels[] = {"16", "5 1/3", "8", "4", "2 2/3", "2", "1 3/5",
				"1 1/3", "1"};
std::string tuning_labels[] = {"Equal tuning",
				"Pythagorean tuning",
				"Wendy Carlos alpha scale"};

int main()
{
	if (setup_organetto())
		return -1;
	if (activate_organetto())
		return -1;
	auto_connect_input();
	auto_connect_output();

	sfg::SFGUI sfgui;
	sfg::Desktop desktop;

	auto drawbars_window = sfg::Window::Create();
	drawbars_window->SetTitle("Drawbars");
	drawbars_window->SetPosition(sf::Vector2f(200.0f, 70.0f));
	auto drawbars_box = sfg::Box::Create();
	drawbars_box->SetSpacing(20.0f);
	for (size_t d = 0; d < HAMMOND_HARMONIC_COUNT; ++d) {
		auto scale_box = sfg::Box::Create(
				sfg::Box::Orientation::VERTICAL);
		auto scale = sfg::Scale::Create(
				sfg::Range::Orientation::VERTICAL);
		scale->SetRequisition(sf::Vector2f(20.0f, 300.0f));
		auto adj = scale->GetAdjustment();
		adj->SetLower(0.0f);
		adj->SetUpper(8.0f);
		adj->SetValue(organetto_hammond_params->amplitudes[d]);
		drawbar_adjs.push_back(adj);
		scale_box->Pack(scale);
		auto label = sfg::Label::Create(drawbar_labels[d]);
		scale_box->Pack(label);
		drawbars_box->Pack(scale_box);
	}
	drawbars_window->Add(drawbars_box);
	desktop.Add(drawbars_window);

	auto tuning_window = sfg::Window::Create();
	tuning_window->SetTitle("Tuning");
	tuning_window->SetPosition(sf::Vector2f(200.0f, 450.0f));
	auto tuning_box = sfg::Box::Create();
	tuning_box->SetSpacing(5.0f);
	auto tuning_choice = sfg::ComboBox::Create();
	for (size_t t = 0; t < ARRAY_SIZE(tuning_labels); ++t) {
		tuning_choice->AppendItem(tuning_labels[t]);
	}
	tuning_choice->SelectItem(organetto_tuning);
	tuning_box->Pack(tuning_choice);
	tuning_window->Add(tuning_box);
	desktop.Add(tuning_window);

	sf::VideoMode mode(800, 600);
	sf::RenderWindow render_window(mode, "Organetto");
	render_window.resetGLStates();

	sf::Clock clock;
	sf::Event event;
	while (render_window.isOpen()) {
		while (render_window.pollEvent(event)) {
			desktop.HandleEvent(event);
			if (event.type == sf::Event::Closed) {
				render_window.close();
			}
		}
		desktop.Update(1.0f);
		render_window.clear();
		sfgui.Display(render_window);
		render_window.display();

		if (clock.getElapsedTime().asMilliseconds() >= 50) {
			for (size_t d = 0; d < HAMMOND_HARMONIC_COUNT; ++d) {
				organetto_hammond_params->amplitudes[d] = 
					drawbar_adjs[d]->GetValue();
			}
			organetto_tuning = static_cast<tuning_system>(
					tuning_choice->GetSelectedItem());
			clock.restart();
		}
	}
	return 0;
}

