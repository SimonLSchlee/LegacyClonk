/*
 * LegacyClonk
 *
 * Copyright (c) RedWolf Design
 * Copyright (c) 2017-2019, The LegacyClonk Team and contributors
 *
 * Distributed under the terms of the ISC license; see accompanying file
 * "COPYING" for details.
 *
 * "Clonk" is a registered trademark of Matthes Bender, used with permission.
 * See accompanying file "TRADEMARK" for details.
 *
 * To redistribute this file separately, substitute the full license texts
 * for the above references.
 */

#include <C4Include.h>
#include <C4EnergyBars.h>
#include <C4Value.h>
#include <C4ValueList.h>
#include <C4ValueHash.h>

#include <C4Game.h>
#ifdef C4ENGINE
#include <C4Def.h>
#include <C4Object.h>
#include <C4Log.h>
#endif
#include <C4Wrappers.h>

#include <string>
#include <vector>
#include <unordered_map>

class C4Object;

// TODO find a place so that this can be shared between c4value.cpp and this file
namespace
{
	// based on boost container_hash's hashCombine
	constexpr void hashCombine(std::size_t &hash, std::size_t nextHash)
	{
		if constexpr (sizeof(std::size_t) == 4)
									 {
#define rotateLeft32(x, r) (x << r) | (x >> (32 - r))
										 constexpr std::size_t c1 = 0xcc9e2d51;
										 constexpr std::size_t c2 = 0x1b873593;

										 nextHash *= c1;
										 nextHash = rotateLeft32(nextHash, 15);
										 nextHash *= c2;

										 hash ^= nextHash;
										 hash = rotateLeft32(hash, 13);
										 hash = hash * 5 + 0xe6546b64;
#undef rotateLeft32
									 }
		else if constexpr (sizeof(std::size_t) == 8)
												{
													constexpr std::size_t m = 0xc6a4a7935bd1e995;
													constexpr int r = 47;

													nextHash *= m;
													nextHash ^= nextHash >> r;
													nextHash *= m;

													hash ^= nextHash;
													hash *= m;

													// Completely arbitrary number, to prevent 0's
													// from hashing to 0.
													hash += 0xe6546b64;
												}
		else
			{
				hash ^= nextHash + 0x9e3779b9 + (hash << 6) + (hash >> 2);
			}
	}
}

C4EnergyBar::C4EnergyBar(int32_t _value, int32_t _max, bool _visible)
	:value(_value), max(_max), visible(_visible)
{}

C4EnergyBars::C4EnergyBars(std::shared_ptr<C4EnergyBarsDef> _def):def(_def) {
	for (auto bardef: def->bars) {
		if (bardef.physical == 0) {
			values.emplace_back(bardef.value, bardef.max, true);
		}
	}
}

void C4EnergyBars::SetEnergyBar(std::string name, int32_t value, int32_t max)
{
	try
	{
		auto index = def->names.at(name);
		auto &bardef = def->bars.at(index);
		if (bardef.value_index >= 0) {
			auto &barval = values.at(bardef.value_index);
			barval.value = value;
			if(max > 0) barval.max = max;
		}
	}
	catch (const std::out_of_range &)
	{
		return;
	}
}

void C4EnergyBars::DrawEnergyBars(C4Facet &cgo, C4Object &obj)
{
	bool fNeedsAdvance = false;
	int32_t iMaxWidth = 0;

	// LogF("drawing energy bars number of bars: %d", def->bars.size());

	for (auto &bardef: def->bars) {
		int32_t value = 0;
		int32_t max   = 0;
		bool fVisible = true;
		bool hideHUDBars = bardef.hide & C4EnergyBarDef::EBH_HideHUDBars;

		switch(bardef.physical) {
		case C4EnergyBarDef::EBP_Energy:
			if (hideHUDBars && obj.Def->HideHUDBars & C4Def::HB_Energy) fVisible = false;
			value = obj.Energy; max = obj.GetPhysical()->Energy; break;
		case C4EnergyBarDef::EBP_Breath:
			if (hideHUDBars && obj.Def->HideHUDBars & C4Def::HB_Breath) fVisible = false;
			value = obj.Breath; max = obj.GetPhysical()->Breath; break;
		case C4EnergyBarDef::EBP_Magic:
			if (hideHUDBars && obj.Def->HideHUDBars & C4Def::HB_MagicEnergy) fVisible = false;
			// draw in units of MagicPhysicalFactor, so you can get a full magic energy bar by script even if partial magic energy training is not fulfilled
			value = obj.MagicEnergy / MagicPhysicalFactor;
			max = obj.GetPhysical()->Magic / MagicPhysicalFactor;
			break;
		default:
			auto &barval = values.at(bardef.value_index);
			value = barval.value;
			max   = barval.max;
			break;
		}

		if (bardef.hide & C4EnergyBarDef::EBH_Empty && value == 0)   fVisible = false;
		if (bardef.hide & C4EnergyBarDef::EBH_Full  && value >= max) fVisible = false;

		// LogF("drawing energy bar: name %s", bardef.name.c_str());
		// LogF("drawing energy bar: value %d max %d", value, max);
		// LogF("drawing energy bar: physical %d hide %d", bardef.physical, bardef.hide);

		if (!fVisible) {
			if (fNeedsAdvance && bardef.advance) {cgo.X += iMaxWidth; iMaxWidth = 0; fNeedsAdvance = false;}
			continue;
		}

		int32_t width = bardef.facet->Wdt;
		cgo.Wdt = width;
		cgo.DrawEnergyLevelEx(value, max, *bardef.facet, bardef.index, bardef.scale);

		iMaxWidth = std::max<int32_t>(iMaxWidth, width+1);
		if (bardef.advance) {cgo.X += iMaxWidth; iMaxWidth = 0; fNeedsAdvance = false;}
		else fNeedsAdvance = true;
	}
}

C4EnergyBarDef::C4EnergyBarDef():
	name(), physical(0), hide(0),
	gfx(), facet(), index(), advance(true),
	value_index(-1), value(0), max(1000000), scale(1.0f)
{}

C4EnergyBarDef::C4EnergyBarDef(const char *_name, const char *_gfx, const std::shared_ptr<C4FacetExID> &_facet, int32_t _index, int32_t _physical):
	name(_name), physical(_physical), hide(DefaultHide(physical)),
	gfx(_gfx), facet(_facet), index(_index), advance(true),
	value_index(-1), value(0), max(1000000), scale(1.0f)
{}

C4EnergyBarDef::~C4EnergyBarDef()
{}

bool C4EnergyBarDef::operator==(const C4EnergyBarDef &rhs) const
{
	return (
		name == rhs.name &&
		physical == rhs.physical &&
		hide == rhs.hide &&
		gfx == rhs.gfx &&
		facet == rhs.facet &&
		index == rhs.index &&
		advance == rhs.advance &&
		value == rhs.value &&
		max == rhs.max
	);
}

int32_t C4EnergyBarDef::PhysicalFromName(std::string name) {
	if(name == "Energy") return EBP_Energy;
	if(name == "Magic")  return EBP_Magic;
	if(name == "Breath") return EBP_Breath;
	return EBP_None;
}

int32_t C4EnergyBarDef::DefaultIndex(int32_t physical) {
	switch (physical) {
	case EBP_Energy: return 0;
	case EBP_Magic:  return 1;
	case EBP_Breath: return 2;
	default:
		return 0;
	}
}

int32_t C4EnergyBarDef::DefaultHide(int32_t physical) {
	switch (physical) {
	case EBP_Energy: return EBH_Never | EBH_HideHUDBars;
	case EBP_Breath: return EBH_EmptyFull | EBH_HideHUDBars; // TODO shouldn't it be only hide full? compare with old code
	case EBP_Magic:
	default:
		return EBH_Empty | EBH_HideHUDBars;
	}
}

std::size_t C4EnergyBarDef::GetHash() const
{
	std::size_t result = std::hash<std::string>{}(name);
	hashCombine(result, std::hash<int32_t>{}(physical));
	hashCombine(result, std::hash<int32_t>{}(hide));
	hashCombine(result, std::hash<std::string>{}(gfx));
	hashCombine(result, std::hash<int32_t>{}(index));
	hashCombine(result, std::hash<bool>{}(advance));
	hashCombine(result, std::hash<int32_t>{}(value_index));
	hashCombine(result, std::hash<int32_t>{}(value));
	hashCombine(result, std::hash<int32_t>{}(max));
	return result;
}

C4EnergyBarsDef::C4EnergyBarsDef(): gfxs(), bars()
{}

C4EnergyBarsDef::C4EnergyBarsDef(const Gfxs &_gfxs, const Bars &_bars): gfxs(_gfxs), bars(_bars)
{
	// TODO exception / log fatal? if returns false?
	PopulateNamesFromValues(bars, names);
}

C4EnergyBarsDef::C4EnergyBarsDef(const Gfxs &_gfxs, const C4EnergyBarsDef::Bars &_bars, const C4EnergyBarsDef::Names &_names): gfxs(_gfxs), bars(_bars), names(_names)
{
}

C4EnergyBarsDef::~C4EnergyBarsDef()
{
}

bool C4EnergyBarsDef::PopulateNamesFromValues(const C4EnergyBarsDef::Bars &bars, C4EnergyBarsDef::Names &names)
{
	int32_t i = 0;
	for (auto bar: bars) {
		auto success = names.emplace(bar.name, i);
		if (!success.second) {
			LogF("C4EnergyBarDef %s definition, names must be unique, duplicate detected", bar.name.c_str());
			return false;
		}
		++i;
	}

	return true;
}

bool C4EnergyBarsDef::operator==(const C4EnergyBarsDef &rhs) const
{
	return map_compare(gfxs, rhs.gfxs) && bars == rhs.bars;
}

std::size_t C4EnergyBarsDef::GetHash() const
{
	std::size_t result = 0;
	for (auto &gfx: gfxs) {
		hashCombine(result, std::hash<uint32_t>{}(gfx.second.key));
		hashCombine(result, std::hash<uint32_t>{}(gfx.second.file));
		hashCombine(result, std::hash<uint32_t>{}(gfx.second.amount));
		hashCombine(result, std::hash<uint32_t>{}(gfx.second.scale));
	}
	for (auto &bardef: bars) {
		hashCombine(result, bardef.GetHash());
	}
	return result;
}

std::size_t std::hash<C4EnergyBarsDef>::operator()(const C4EnergyBarsDef &value) const noexcept
{
	return value.GetHash();
}

std::shared_ptr<C4FacetExID> C4EnergyBarsUniquifier::GetFacet(C4EnergyBarsDef::Gfxs &gfxs, const char *gfx)
{
	std::string key(gfx);

	try {
		auto facet = graphics.at(key).lock();
		if(facet) {
			return facet;
		}
	} catch (const std::out_of_range &) {
	}

	// facet needs to be loaded
	int32_t amount = 0;
	int32_t scale  = 100;
	std::string file;
	try {
		auto gfx = gfxs.at(key);
		amount = gfx.amount;
		scale = gfx.scale;
		file = gfx.file;
	} catch (const std::out_of_range &) {
		LogF("Missing key '%s' in graphics definition", key.c_str());
		return nullptr;
	}

	// TODO FIXME why wasn't this already called before we get here?
	Game.GraphicsResource.RegisterMainGroups();

	auto facet = std::shared_ptr<C4FacetExID>(new C4FacetExID(), [=](C4FacetExID* facet){graphics.erase(key);});
	bool success = Game.GraphicsResource.LoadFile(*facet, file.c_str(), Game.GraphicsResource.Files);
	if(!success) {LogF("could not load custom energy bar graphic '%s'", file.c_str()); return nullptr;}

	int32_t bar_wdt = facet->Surface->Wdt / (amount * 2);
	int32_t bar_hgt = facet->Surface->Hgt / 3;
	bar_wdt = (bar_wdt*100) / scale;
	bar_hgt = (bar_hgt*100) / scale;
	facet->Set(facet->Surface, 0, 0, bar_wdt, bar_hgt);

	graphics.emplace(key, std::weak_ptr<C4FacetExID>(facet));
	return facet;
}

std::shared_ptr<C4EnergyBarsDef> C4EnergyBarsUniquifier::UniqueifyDefinition(C4EnergyBarsDef *definition)
{
	// TODO somehow change *definition to unique_ptr?
	// currently definition always gets owned by a shared_ptr, that either ends up being the canonical shared_ptr
	// or goes out of scope deleting the definition

	// the weak ptr remembers the custom deleter
	auto shared = std::shared_ptr<C4EnergyBarsDef>(definition, [=](C4EnergyBarsDef* def){definitions.erase(*def);});
	auto it_success = definitions.emplace(*definition, std::weak_ptr<C4EnergyBarsDef>(shared));
	if (!it_success.second) {
		// definition already existed, we have to override shared so that it is linked to
		// the same control block that was used to create the first shared ptr
		shared = it_success.first->second.lock();
	}
	return shared;
}

std::shared_ptr<C4EnergyBars> C4EnergyBarsUniquifier::DefineEnergyBars(C4ValueHash* graphics, C4ValueArray *definition)
{
	if (!graphics) {
		LogF("DefineEnergyBars Parameter 1 map expected but got: %s", C4Value(graphics).GetDataString().getData());
		return nullptr;
	}

	if (!definition) {
		LogF("DefineEnergyBars Parameter 2 array expected but got: %s", C4Value(definition).GetDataString().getData());
		return nullptr;
	}

	int32_t value_index = 0;
	C4EnergyBarsDef::Gfxs gfx;
	C4EnergyBarsDef::Bars bars;
	C4EnergyBarsDef::Names names;
	bool success = ProcessGraphics(*graphics, gfx) && ProcessGroup(value_index, gfx, *definition, bars, true) && C4EnergyBarsDef::PopulateNamesFromValues(bars, names);
	if(success) {
		LogF("DefineEnergyBars success");
		auto def = UniqueifyDefinition(new C4EnergyBarsDef(gfx, bars, names));
		return Instantiate(def);
	}
	LogF("DefineEnergyBars failed returning nullptr");
	return nullptr;
}

bool C4EnergyBarsUniquifier::ProcessGraphics(C4ValueHash &map, C4EnergyBarsDef::Gfxs &gfx)
{
	auto amount = C4VString("amount");
	auto scale  = C4VString("scale");

	LogF("processing graphics %s ", C4Value(&map).GetDataString().getData());

	C4ValueHash::Iterator end = map.end();
	for (C4ValueHash::Iterator it = map.begin(); it != end; ++it) {
		C4Value key = (*it).first;
		auto _key = key.getStr()->Data.getData();

		C4Value val = (*it).second;
		auto *_val = val.getMap();
		auto &m = *_val;

		C4Value file = m[C4VString("file")];
		auto _file = _key;
		if (file != C4VNull) _file = file.getStr()->Data.getData();

		int32_t _amount = m[amount].getInt();
		int32_t _scale  = m[scale].getInt();
		if (_amount == 0) _amount = 1;
		if (_scale == 0) _scale = 100;

		LogF("processing graphics amount %d scale %d ", _amount, _scale);
		auto it_success = gfx.emplace(_key, C4EnergyBarsDef::Gfx(_key, _file, _amount, _scale));
		if (!it_success.second) {
			LogF("DefineEnergyBars %s duplicate key in gfx description ", _key);
			return false;
		}
	}

	return true;
}

bool C4EnergyBarsUniquifier::ProcessGroup(int32_t &value_index, C4EnergyBarsDef::Gfxs &graphics, const C4ValueArray &group, C4EnergyBarsDef::Bars &bars, bool advanceAlways)
{
	LogF("ProcessingGroup of Size: %d", group.GetSize());
	int32_t size = group.GetSize();
	for (int32_t i = 0; i < size; ++i) {
		C4Value element = group[i];
		switch (element.GetType()) {
		case C4V_Map:
		{
			auto *map = element.getMap();
			if (map) {
				if(!ProcessEnergyBar(value_index, graphics, *map, bars, advanceAlways || i == size-1)) return false;
			} else {
				LogF("DefineEnergyBars got unexpected value: %s", element.GetDataString().getData());
				return false;
			}
			break;
		}
		case C4V_Array:
			if(advanceAlways) {
				auto *array = element.getArray();
				if (array) {
					if(!ProcessGroup(value_index, graphics, *array, bars, false)) return false;
				} else {
					LogF("DefineEnergyBars got unexpected value: %s", element.GetDataString().getData());
					return false;
				}
			} else {
				LogF("DefineEnergyBars groups in groups are not allowed: %s", element.GetDataString().getData());
				return false;
			}
			break;

		default:
			LogF("DefineEnergyBars array or map expected but got: %s", element.GetDataString().getData());
			return false;
			break;
		}
	}
	return true;
}

bool C4EnergyBarsUniquifier::ProcessEnergyBar(int32_t &value_index, C4EnergyBarsDef::Gfxs &graphics, const C4ValueHash &bar, C4EnergyBarsDef::Bars &bars, bool advance)
{
	C4Value name = bar[C4VString("name")];
	auto *_name = name.getStr();
	if (!_name) {
		LogF("DefineEnergyBars: EnergyBar definition has no name: %s", name.GetDataString().getData());
		return false;
	}
	LogF("Processing EnergyBar 1: %s", name.GetDataString().getData());

	C4Value gfx = bar[C4VString("gfx")];
	auto *_gfx = gfx.getStr();
	if (!_gfx) {
		LogF("DefineEnergyBars: EnergyBar definition has no gfx: %s", gfx.GetDataString().getData());
		return false;
	}
	LogF("Processing EnergyBar 2: %s", gfx.GetDataString().getData());

	C4Value physical = bar[C4VString("physical")];
	C4ValueInt _physical = physical.getInt();
	if (_physical & ~C4EnergyBarDef::EBP_All) {
		LogF("DefineEnergyBars %s definition has invalid physical: %s", name.GetDataString().getData(), physical.GetDataString().getData());
		return false;
	}
	LogF("Processing EnergyBar 3: %s _physical %d", name.GetDataString().getData(), _physical);

	C4Value hide = bar[C4VString("hide")];
	C4ValueInt _hide = C4EnergyBarDef::EBH_Empty;
	if (hide != C4VNull) _hide = hide.getInt();
	if (_hide & ~C4EnergyBarDef::EBH_All) {
		LogF("DefineEnergyBars %s definition has invalid hide: %s", name.GetDataString().getData(), hide.GetDataString().getData());
		return false;
	}
	LogF("Processing EnergyBar 4: %s _hide %d", name.GetDataString().getData(), _hide);

	C4Value index = bar[C4VString("index")];
	C4Value value = bar[C4VString("value")];
	C4Value max   = bar[C4VString("max")];
	C4ValueInt _index = index.getInt();
	C4ValueInt _value = value.getInt();
	C4ValueInt _max = max.getInt();
	if (_index < 0) {LogF("DefineEnergyBars %s definition has invalid index: %s", name.GetDataString().getData(), _index); return false;}
	if (_value < 0) {LogF("DefineEnergyBars %s definition has invalid value: %s", name.GetDataString().getData(), _value); return false;}
	if (_max   < 0) {LogF("DefineEnergyBars %s definition has invalid max: %s", name.GetDataString().getData(), _max); return false;}
	LogF("Processing EnergyBar 5: %s", name.GetDataString().getData());

	LogF("Processing EnergyBar 6: %s _index %d _value %d _max %d", name.GetDataString().getData(), _index, _value, _max);

	// const C4Facet &gfx = Def->GetMainFace(&Def->Graphics);

	{
		const char* file = _gfx->Data.getData();
		Log("facet loading...");
		auto facet = GetFacet(graphics, file);
		if(facet == nullptr) {return false;}

		C4EnergyBarDef bar(_name->Data.getData(), file, facet, _index, _physical);
		if(physical != C4VNull) {
			bar.physical = _physical;
			bar.hide = C4EnergyBarDef::DefaultHide(_physical);
		} else {
			bar.value_index = value_index++;
		}
		if(hide != C4VNull) bar.hide = _hide;
		bar.value = _value;
		bar.max = _max;
		bar.advance = advance;
		auto scale = graphics.at(file).scale;
		bar.scale = static_cast<float>(scale) / 100.0f;
		bars.push_back(bar);
	}

	LogF("Processing EnergyBar 7: %s return true", name.GetDataString().getData());
	return true;
}

std::shared_ptr<C4EnergyBars> C4EnergyBarsUniquifier::Instantiate(std::shared_ptr<C4EnergyBarsDef> definition)
{
	if(definition == nullptr) return nullptr;
	return std::make_shared<C4EnergyBars>(definition);
}

void C4EnergyBarsAdapt::CompileFunc(StdCompiler *pComp) {
	bool fCompiler = pComp->isCompiler();

	if (!fCompiler) {
		// serialize
		if (pBars) {
			std::vector<C4EnergyBarsDef::Gfx> temp;
			for (auto it: pBars->def->gfxs) {
				temp.push_back(it.second);
			}
			pComp->Value(mkNamingAdapt(mkSTLContainerAdapt(temp), "Gfx", std::vector<C4EnergyBarsDef::Gfx>{}));
			pComp->Value(mkNamingAdapt(mkSTLContainerAdapt(pBars->def->bars), "Def", C4EnergyBarsDef::Bars{}));
			pComp->Value(mkNamingAdapt(mkSTLContainerAdapt(pBars->values), "Bar", std::vector<C4EnergyBar>{}));
		}

	} else {
		// deserialize
		C4EnergyBarsDef::Gfxs gfxs{};
		std::vector<C4EnergyBarsDef::Gfx> temp;
		pComp->Value(mkNamingAdapt(mkSTLContainerAdapt(temp), "Gfx", std::vector<C4EnergyBarsDef::Gfx>{}));
		for (auto &gfx: temp) {
			gfxs.emplace(gfx.key, gfx);
		}

		C4EnergyBarsDef::Bars bars{};
		pComp->Value(mkNamingAdapt(mkSTLContainerAdapt(bars), "Def", C4EnergyBarsDef::Bars{}));

		C4EnergyBarsDef *def = new C4EnergyBarsDef();
		def->gfxs.swap(gfxs);
		def->bars.swap(bars);

		// get facets and restore scale from gfxs
		for (auto &bar: def->bars) {
			bar.facet = Game.EnergyBars.GetFacet(def->gfxs, bar.gfx.c_str());
			auto scale = def->gfxs.at(bar.gfx).scale;
			bar.scale = static_cast<float>(scale) / 100.0f;
		}

		auto uniq_def = Game.EnergyBars.UniqueifyDefinition(def);
		auto instance = Game.EnergyBars.Instantiate(uniq_def);
		pComp->Value(mkNamingAdapt(mkSTLContainerAdapt(instance->values), "Bar", std::vector<C4EnergyBar>{}));

		pBars = instance;
	}
}

